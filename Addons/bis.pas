unit bis;
// Упаковщик изображений BIS V1.23
// Image compression utility BIS v1.23
// (C) 2004 Miek
// Freeware

// Предназначен для сжатия изображений, имеющих большие области, 
// заполненные одним цветом

// Intended for compressing images with large areas filled by single colour


// Достоинства:
// Advantages:

// + неограниченный коэффициент сжатия (пустые области любого размера
//   могут быть сжаты в несколько бит)
// + unlimited compression ratio (empty areas of any size can be compressed
//   into several bits)

// + поддерживает форматы от 1 бит на пиксел до 32 бит на пиксел
// + supports any image formats from 1 bit per pixel to 32 bits per pixel

// + малый объем кода
// + small code

// + неплохая скорость распаковки
// + quick uncompression

// Недостатки:
// Disadvantages:

// - чувствителен к любым шумам (неоднородностям) в изображении
// - any noise (irregularity) in the source image lower the compression
//   ratio considerably

// - низкая скорость упаковки
// - low compression speed

// - в данный момент не является thread-safe, т.е не может быть запущен
//   одновременно из нескольких потоков одной программы
// - current version is not thread-safe


interface

uses
  windows, KOL, KOLadd, KOLZLib;
  // Выходной двоичный поток компрессора направляется в ZIP-упаковщик.
  // В принципе можно ZLIB заменить на любой другой модуль архивации, если он
  // поддерживает последовательные операции с потоками.

  // The output stream of the compressor goes directly to the ZIP compressor.
  // Any compression module which use similar interface (stream writing
  // and reading) can be used inistead of ZLIB.

const
  BISmagic = $1200FADE; // сигнатура BIS-файла V1.2
                        // BIS file signature V1.2

  BISversion = '1.23';  // версия BIS
                        // program version

  beOK               = 0;
  beWriteError       = 1;
  beReadError        = 2;
  beWrongFileFormat  = 3;
  beWrongImageFormat = 4;
  beUnknownError     = 5;

  // сжать в существующий поток
  // compress an image into a stream
  procedure BISCompressToStream( source: pbitmap; var dst: pstream);

  // распаковать из существующего потока
  // decompress an image from a stream
  function BISDecompressFromStream( srcstream: pstream): pbitmap;

  // сжать в файл
  // compress an image into a file
  procedure BISCompressToFile( source: pbitmap; dstfilename: string);

  // распаковать из файла
  // compress an image from a file
  function BISDecompressFromFile( sourcefilename: string): pbitmap;

  // получить расшифровку текущего кода ошибки
  // convert the current BIS error-code into a text string
  function GetErrorString( errcode: integer): string;

var
  LastBISError: integer;

implementation

// sorry, further comments only in Russian!

type
  // К сожалению, TBits (пока) не имеет средств записи в поток
  // Приходится изворачиваться самому
  PBitsEx= ^TBitsEx;
  TBitsEx = object( tbits)
    procedure SaveToStream( dst: pstream);
    function  LoadFromStream( src: pstream): boolean;
  end;

  // идентификатор результата анализа региона:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  // zUncompressed - регион не удалось сжать и он записан напрямую
  // zMixed - регион содержит области разного типа
  // zOne - регион заполнен единицами
  // zZero - регион заполнен нулями
  packresult = (zUncompressed=0, zMixed=1, zZero=2, zOne=3);

  // типы данных для более эффективного использования стека
  tsmallpoint= packed record
    x, y: smallint;
  end;

  tsmallrectrec = packed record
    a, b: tsmallpoint;
  end;

  tsmallrect = int64;

var
  src: pbitmap;
  data: PBitsEx;
  bitindex: integer;

  scanline0: pointer;
  scanlinedelta: integer;

function GetErrorString;
begin
  case errcode of
    beOK: result:= 'No error';
    beWriteError: result:= 'Stream write error';
    beReadError: result:= 'Stream read error';
    beWrongFileFormat: result:= 'Wrong file format';
    beWrongImageFormat: result:= 'Wrong source image format';
    else result:= 'Unknown error';
  end;
end;

{$R-}
// преобразовать две точки в упакованный прямоугольник
function packrect( a, b: tsmallpoint): tsmallrect;
begin
  tsmallrectrec( result).a:= a;
  tsmallrectrec( result).b:= b;
end;

// переместить указатель в TBitsEx на начало
procedure rewind;
begin
  bitindex:= 0;
end;

// Взять следующий бит из потока
function peekbit: boolean;
begin
  result:= data.Bits[ bitindex];
  inc( bitindex);
end;

// Добавить бит в поток
procedure addbit( x: boolean);
begin
  data.Bits[ bitindex]:= x;
  inc( bitindex);
end;

// записать в поток результат анализа региона
procedure writeresult( x: packresult);
begin
  case x of
    zUncompressed:
      begin
        addbit( false);
        addbit( false);
      end;
    zMixed:
      begin
        addbit( false);
        addbit( true);
      end;
    zZero:
      begin
        addbit( true);
        addbit( false);
      end;
    zOne:
      begin
        addbit( true);
        addbit( true);
      end;
  end;
end;

// Используемый метод я пока не встречал в литературе, поэтому условно
// называю его методом бинарной декомпозиции (binary image splitting).
// ---------------------------------------------------------------------
// Суть компрессии в следующем.
// -----------------------------
// Сжимается монохромное двумерное изображение. Если в исходном изображении
// глубина цвета больше, нужно его сначала разбить на битовые плоскости.

// В рекурсивную процедуру передаются координаты сжимаемого региона (прямоугольника).
// Если регион пустой - заполнен одним цветом - в выходной поток будет записан
// код цвета. Если регион не пустой, в поток пишется соотв.признак, после чего
// регион разбивается на части (2 или 4, в зависимости от размера) и для каждой
// процедура вызывается рекурсивно. Узнать, пустой регион или нет, можно только
// после спуска в рекурсию, разумеется.

// Когда размер региона дойдет до 1x1, содержимое региона будет
// определено однозначно и рекурсия пойдет назад.
// Во время рекурсивного анализа определяются случаи, когда все регионы, на
// которые был разбит регион-источник, оказались заполненными одним
// определенным цветом. В этом случае в поток не пишется ничего, предоставляя
// это более верхнему уровню рекурсии. При записи также определяется факт, что
// содержимое региона не удалось сжать - в этом случае записывает соотв.признак
// и следом напрямую содержимое региона.

// P.S. Поскольку разбиение регионов на части однозначное, то их координаты
// никуда записывать не придется.


// запаковать регион в поток. Процедура рекурсивная, теоретически возможно
// переполнение стека, впрочем, такого пока не случалось
function pack( z: tsmallrect): packresult;
var
  j, i: Integer;
  rind: integer;
  lt, lb, rt, rb: tsmallrectrec;
  results: packed record
    rlt, rlb, rrt, rrb: packresult;
  end;
begin
  result:= zMixed;
  if lastbiserror<>beUnknownError then exit;

  // запомнить положение указателя в потоке
  rind:= bitindex;
  with tsmallrectrec( z), results do
    begin
      // если размер региона один пиксел, то его содержимое однозначно известно
      // и здесь рекурсию надо отматывать назад
      if (b.x=a.x) and (b.y=a.y) then
        begin
          // взять пиксел через прямой доступ в память
          if src.dibpixels[ a.x, a.y]=0 then
//          if byte( pointer( integer( scanline0)+a.y*scanlinedelta+a.x shr 3)^) and (1 shl (7-a.x and 7))=0 then
            result:= zZero
          else
            result:= zOne;
        end
      else
        begin
          // иначе нужно разбить регион на части и для каждой сделать
          // рекурсивный вызов
          addbit( false); // временно пишем код zMixed
          addbit( true);
          lt:= tsmallrectrec( z);
          rt:= tsmallrectrec( z);
          lb:= tsmallrectrec( z);
          rb:= tsmallrectrec( z);
          if b.x-a.x>0 then
            begin
              if b.y-a.y>0 then
                begin
                  // разбиваем на четыре части
                  lt.b.x:= lt.a.x+(lt.b.x-lt.a.x) div 2;
                  rt.a.x:= lt.b.x+1;
                  lb.b.x:= lt.b.x;
                  rb.a.x:= rt.a.x;

                  lt.b.y:= lt.a.y+(lt.b.y-lt.a.y) div 2;
                  lb.a.y:= lt.b.y+1;
                  rt.b.y:= lt.b.y;
                  rb.a.y:= lb.a.y;
                  // анализ региона
                  rlt:= pack( tsmallrect( lt));
                  // проанализировали регион: если он смешанный, то ничего не
                  // записываем в поток, потому что это уже сделал
                  // более глубокий уровень рекурсии
                  if rlt<>zMixed then
                    begin
                      // один маленький трюк: если регион сократился до одного
                      // пиксела, будем его код записывать одним битом, а не
                      // двумя!
                      if (lt.a.x=lt.b.x) and (lt.a.y=lt.b.y) then
                        addbit( rlt=zOne)
                      else
                        writeresult( rlt);
                    end;

                  rrt:= pack( tsmallrect( rt));
                  if rrt<>zMixed then
                    begin
                      if (rt.a.x=rt.b.x) and (rt.a.y=rt.b.y) then
                        addbit( rrt=zOne)
                      else
                        writeresult( rrt);
                    end;
                  rlb:= pack( tsmallrect( lb));
                  if rlb<>zMixed then
                    begin
                      if (lb.a.x=lb.b.x) and (lb.a.y=lb.b.y) then
                        addbit( rlb=zOne)
                      else
                        writeresult( rlb);
                    end;
                  rrb:= pack( tsmallrect( rb));
                  if rrb<>zMixed then
                    begin
                      if (rb.a.x=rb.b.x) and (rb.a.y=rb.b.y) then
                        addbit( rrb=zOne)
                      else
                        writeresult( rrb);
                    end;

                  // если все части определены и одинаковы, ничего в поток
                  // писать не надо. Но туда уже записано заранее, поэтому
                  // просто возвращаем указатель на сохраненную позицию.
                  if (rlt=rrt) and (rlb=rrb) and (rlb=rrt) and (rlt<>zMixed) then
                    begin
                      result:= rlt;
                      bitindex:= rind;
                    end
                  else
                    begin
                      result:= zMixed;
                      // а если все-таки сжатый размер региона больше, чем
                      // исходный, то запишем код zUncompressed и следом
                      // "прямым текстом" это несжимаемое содержимое.
                      if (bitindex-rind)>(b.x-a.x+1)*(b.y-a.y+1) then
                        begin
                          bitindex:= rind;
                          writeresult( zUncompressed);
                          for j:= a.y to b.y do
                            begin
                              integer( lt.a):= integer( scanline0)+j*scanlinedelta;
                              for i:= a.x to b.x do
                                addbit( byte( pointer( integer( lt.a)+i shr 3)^) and (1 shl (7-i and 7))<>0);
                            end;
                        end;
                    end;
                end
              else
                begin
                  // разбиваем на две части по горизонтали
                  lt.b.x:= lt.a.x+(lt.b.x-lt.a.x) div 2;
                  rt.a.x:= lt.b.x+1;
                  rlt:= pack( tsmallrect( lt));
                  if rlt<>zMixed then
                    begin
                      if (lt.a.x=lt.b.x) and (lt.a.y=lt.b.y) then
                        addbit( rlt=zOne)
                      else
                        writeresult( rlt);
                    end;
                  rrt:= pack( tsmallrect( rt));
                  if rrt<>zMixed then
                    begin
                      if (rt.a.x=rt.b.x) and (rt.a.y=rt.b.y) then
                        addbit( rrt=zOne)
                      else
                        writeresult( rrt);
                    end;

                  if (rlt=rrt) and (rlt<>zMixed) then
                    begin
                      result:= rlt;
                      bitindex:= rind;
                    end
                  else
                    begin
                      result:= zMixed;
                      if (bitindex-rind)>(b.x-a.x+1)*(b.y-a.y+1) then
                        begin
                          bitindex:= rind;
                          writeresult( zUncompressed);
                          for j:= a.y to b.y do
                            begin
                              integer( lt.a):= integer( scanline0)+j*scanlinedelta;
                              for i:= a.x to b.x do
                                addbit( byte( pointer( integer( lt.a)+i shr 3)^) and (1 shl (7-i and 7))<>0);
                            end;
                        end;
                    end;
                end;
            end
          else
            begin
              // разбиваем на две части по вертикали
              lt.b.y:= lt.a.y+(lt.b.y-lt.a.y) div 2;
              lb.a.y:= lt.b.y+1;
              rlt:= pack( tsmallrect( lt));
              if rlt<>zMixed then
                begin
                  if (lt.a.x=lt.b.x) and (lt.a.y=lt.b.y) then
                    addbit( rlt=zOne)
                  else
                    writeresult( rlt);
                end;
              rlb:= pack( tsmallrect( lb));
              if rlb<>zMixed then
                begin
                  if (lb.a.x=lb.b.x) and (lb.a.y=lb.b.y) then
                    addbit( rlb=zOne)
                  else
                    writeresult( rlb);
                end;

              if (rlt=rlb) and (rlt<>zMixed) then
                begin
                  result:= rlt;
                  bitindex:= rind;
                end
              else
                begin
                  result:= zMixed;
                  if (bitindex-rind)>(b.x-a.x+1)*(b.y-a.y+1) then
                    begin
                      bitindex:= rind;
                      writeresult( zUncompressed);
                      for j:= a.y to b.y do
                        begin
                          integer( lt.a):= integer( scanline0)+j*scanlinedelta;
                          for i:= a.x to b.x do
                            addbit( byte( pointer( integer( lt.a)+i shr 3)^) and (1 shl (7-i and 7))<>0);
                        end;
                    end;
                end;
            end;
          end;
    end;
end;

procedure unpack( z: tsmallrect);
var
  lt, lb, rt, rb: tsmallrectrec;
  i, j: integer;
begin
  if lastbiserror<>beUnknownError then exit;

  with tsmallrectrec( z) do
    begin
      // если регион сократился до одного пиксела, взять один бит
      if (b.x=a.x) and (b.y=a.y) then
        begin
          if peekbit then
          // установить пиксел через прямой доступ в память
            src.DIBPixels[ a.x, a.y]:= clwhite;
        end
      else
        begin
          // берем первый бит 2-битного кода
          if peekbit then
            begin
              // код zOne или zZero: регион заполнен одним цветом
              if peekbit then
                patblt( src.canvas.Handle, a.x, a.y, b.x-a.x+1, b.y-a.y+1, whiteness)
            end
          else
            begin
              if not peekbit then
                // код zUncompressed: несжатый регион
                begin
                  for j:= a.y to b.y do
                    for i:= a.x to b.x do
                      if peekbit then src.DIBPixels[ i, j]:= clwhite;
                end
              else
                begin
                  // код zMixed: регион смешанный, углубить рекурсию
                  lt:= tsmallrectrec( z);
                  rt:= tsmallrectrec( z);
                  lb:= tsmallrectrec( z);
                  rb:= tsmallrectrec( z);
                  if b.x-a.x>0 then
                    begin
                      if b.y-a.y>0 then
                        begin
                          lt.b.x:= lt.a.x+(lt.b.x-lt.a.x) div 2;
                          lt.b.y:= lt.a.y+(lt.b.y-lt.a.y) div 2;
                          unpack( tsmallrect( lt));

                          rt.a.x:= lt.b.x+1;
                          rt.b.y:= lt.b.y;
                          unpack( tsmallrect( rt));

                          lb.b.x:= lt.b.x;
                          lb.a.y:= lt.b.y+1;
                          unpack( tsmallrect( lb));

                          rb.a.y:= lb.a.y;
                          rb.a.x:= rt.a.x;
                          unpack( tsmallrect( rb));
                        end
                      else
                        begin
                          lt.b.x:= lt.a.x+(lt.b.x-lt.a.x) div 2;
                          unpack( tsmallrect( lt));

                          rt.a.x:= lt.b.x+1;
                          unpack( tsmallrect( rt));
                        end;
                    end
                  else
                    begin
                      lt.b.y:= lt.a.y+(lt.b.y-lt.a.y) div 2;
                      unpack( tsmallrect( lt));

                      lb.a.y:= lt.b.y+1;
                      unpack( tsmallrect( lb));
                    end;
                end;
            end;
        end;
    end;
end;

function NewBitsEx: PBitsEx;
begin
  result:= pbitsex( newbits);
  result.Capacity:= 50000*8;
  bitindex:= 0;
end;

function TBitsEx.LoadFromStream;
var
  i: integer;
begin
  result:= false;

  if src.Read( i, sizeof( i))<>sizeof( i) then
    lastbiserror:= beReadError;
  if i<1 then exit;
  self.bits[ i]:= false;

  if src.Read( self.fList.datamemory^, (i+7) shr 3)<>cardinal( (i+7) shr 3) then
    lastbiserror:= beReadError;
  self.fCount:= i;
  result:= true;
end;

procedure TBitsEx.SaveToStream( dst: pstream);
var
  i: integer;
begin
  i:= bitindex;
  if dst.Write( i, sizeof( i))<>sizeof( i) then
    lastbiserror:= beWriteError;
  if dst.Write( self.fList.datamemory^, (i+7) shr 3)<>cardinal( (i+7) shr 3) then
    lastbiserror:= beWriteError;
end;

procedure getmask( src, dst: pbitmap; digit: integer);
var
  i, j, k, l: integer;
  ptr, p, psrc: pointer;
begin
  k:= 1;
  for i:= 1 to digit do
    k:= k shl 1;

  scanline0:= src.ScanLine[ 0];
  scanlinedelta:= integer( src.ScanLine[ 1])-integer( ScanLine0);
  ptr:= scanline0;
  case src.pixelformat of
    pf1bit:
      for j:= 0 to src.Height-1 do
        begin
          p:= ptr;
          psrc:= dst.ScanLine[ j];
          move( p^, psrc^, (src.Width+7) shr 3);
          inc( integer( ptr), scanlinedelta);
        end;
    pf4bit:
      for j:= 0 to src.Height-1 do
        begin
          psrc:= dst.ScanLine[ j];
          p:= ptr;
          for i:= 0 to src.width-1 do
            begin
              if i and 1=0 then
                l:= byte( p^) shr 4
              else
                begin
                  l:= byte( p^) and $F;
                  inc( integer( p));
                end;
              if l and k<>0 then
                byte( psrc^):= byte( psrc^) or (1 shl ((7-i) and 7));
              if (i and 7)=7 then inc( integer( psrc));
            end;
          inc( integer( ptr), scanlinedelta);
        end;
    pf8bit:
      for j:= 0 to src.Height-1 do
        begin
          psrc:= dst.ScanLine[ j];
          p:= ptr;
          digit:= 0;
          for i:= 0 to src.width-1 do
            begin
              if byte( p^) and k<>0 then
                digit:= digit or (1 shl ((7-i) and 7));
              inc( integer( p));
              if (i and 7)=7 then
                begin
                  if digit<>0 then
                    byte( psrc^):= lo( digit);
                  inc( integer( psrc));
                  digit:= 0;
                end;
            end;
          if ((i and 7)<>7) and (digit<>0) then byte( psrc^):= digit;
          inc( integer( ptr), scanlinedelta);
        end;
    pf15bit, pf16bit:
      for j:= 0 to src.Height-1 do
        begin
          p:= ptr;
          psrc:= dst.ScanLine[ j];
          digit:= 0;
          for i:= 0 to src.width-1 do
            begin
              if word( p^) and k<>0 then
                digit:= digit or (1 shl ((15-i) and 15));
              inc( integer( p), 2);
              if (i and 15)=15 then
                begin
                  if digit<>0 then
                    word( psrc^):= digit;
                  inc( integer( psrc), 2);
                  digit:= 0;
                end;
            end;
          if ((i and 15)<>15) and (digit<>0) then word( psrc^):= digit;
          inc( integer( ptr), scanlinedelta);
        end;
    pf24bit:
      begin
        for j:= 0 to src.Height-1 do
          begin
            p:= ptr;
            psrc:= dst.ScanLine[ j];
            for i:= 0 to src.width-2 do
              begin
                if dword( p^) and k<>0 then
                  byte( psrc^):= byte( psrc^) or (1 shl ((7-i) and 7));
                inc( integer( psrc), ord( (i and 7)=7));
                inc( integer( p), 3);
              end;
            if src.DIBPixels[ src.width-1, j] and k<>0 then
              byte( psrc^):= byte( psrc^) or (1 shl ((7-(src.width-1)) and 7));
          inc( integer( ptr), scanlinedelta);
        end;
      end;
    pf32bit:
      for j:= 0 to src.Height-1 do
        begin
          p:= ptr;
          psrc:= dst.ScanLine[ j];
          digit:= 0;
          for i:= 0 to src.width-1 do
            begin
              if dword( p^) and k<>0 then
                digit:= digit or (1 shl ((31-i) and 31));
              inc( integer( p), 4);
              if (i and 31)=31 then
                begin
                  dword( psrc^):= digit;
                  inc( integer( psrc), 4);
                  digit:= 0;
                end;
            end;
          if ((i and 31)<>31) and (digit<>0) then dword( psrc^):= digit;
          inc( integer( ptr), scanlinedelta);
        end;
  end;
end;

procedure putmask( src, dst: pbitmap; digit: integer);
var
  i, j: integer;
  k: cardinal;
  p, psrc, ptr: pointer;

begin
  k:= 1;
  for i:= 1 to digit do
    k:= k shl 1;

  ptr:= scanline0;
  case dst.BitsPerPixel of
    1:
      begin
        digit:= (src.Width+7) shr 3;
        for j:= 0 to src.Height-1 do
          begin
            psrc:= ptr;
            p:= dst.ScanLine[ j];
            move( psrc^, p^, digit);
            inc( integer( ptr), scanlinedelta);
          end;
      end;
    4:
      for j:= 0 to src.Height-1 do
        begin
          p:= dst.ScanLine[ j];
          psrc:= ptr;
          for i:= 0 to src.width-1 do
            begin
              if byte( psrc^) and (1 shl ((7-i) and 7))<>0 then
                begin
                  if i and 1=0 then
                    byte( p^):= byte( p^) or (k shl 4)
                  else
                    byte( p^):= byte( p^) or k;
                end;
              inc( integer( p), ord( i and 1<>0));
              inc( integer( psrc), ord( (i and 7)=7));
            end;
          inc( integer( ptr), scanlinedelta);
        end;
    8:
      for j:= 0 to src.Height-1 do
        begin
          p:= dst.ScanLine[ j];
          psrc:= ptr;
          digit:= byte( psrc^);
          for i:= 0 to src.width-1 do
            begin
              if digit and (1 shl ((7-i) and 7))<>0 then
                byte( p^):= byte( p^) or k;
              inc( integer( p));
              if (i and 7=7) then
                begin
                  inc( integer( psrc));
                  if i<src.width-1 then digit:= byte( psrc^);
                end;
            end;
          inc( integer( ptr), scanlinedelta);
        end;
    15, 16:
      for j:= 0 to src.Height-1 do
        begin
          p:= dst.ScanLine[ j];
          psrc:= ptr;
          digit:= word( psrc^);
          for i:= 0 to src.width-1 do
            begin
              if digit and (1 shl ((15-i) and 15))<>0 then
                word( p^):= word( p^) or k;
              inc( integer( p), 2);
              if (i and 15)=15 then
                begin
                  inc( integer( psrc), 2);
                  if i<src.width-1 then digit:= word( psrc^);
                end;
            end;
          inc( integer( ptr), scanlinedelta);
        end;
    24:
      for j:= 0 to src.Height-1 do
        begin
          p:= dst.ScanLine[ j];
          psrc:= ptr;
          for i:= 0 to src.width-2 do
            begin
              if byte( psrc^) and (1 shl ((7-i) and 7))<>0 then
                dword( p^):= dword( p^) or k;
              inc( integer( psrc), ord( (i and 7)=7));
              inc( integer( p), 3);
            end;
          if byte( psrc^) and (1 shl ((7-(src.width-1)) and 7))<>0 then
            dword( pointer( integer( p)-1)^):= dword( pointer( integer( p)-1)^) or (k shl 8);
          inc( integer( ptr), scanlinedelta);
        end;
    32:
      for j:= 0 to src.Height-1 do
        begin
          p:= dst.ScanLine[ j];
          psrc:= ptr;
          digit:= dword( psrc^);
          for i:= 0 to src.width-1 do
            begin
              if digit and (1 shl ((31-i) and 31))<>0 then
                dword( p^):= dword( p^) or k;
              inc( integer( p), 4);
              if (i and 31)=31 then
                begin
                  inc( integer( psrc), 4);
                  if i<src.width-1 then digit:= dword( psrc^);
                end;
            end;
          inc( integer( ptr), scanlinedelta);
        end;
  end;
end;

procedure BIScompresstostream;
var
  tmpbitmap: array[ 0..31] of pbitmap;
  i, bpp: integer;
  m, n: tsmallpoint;
  zipper: pstream;
begin
  lastbiserror:= beUnknownError;
  if (source=nil) or (source.width>32766) or (source.height>32766) or
    (source.PixelFormat=pfDevice) then
    begin
      lastbiserror:= beWrongImageFormat;
      exit;
    end;
  if not NewZLibCStream( Zipper, clMax, Dst, nil) then exit;

  i:= source.Width;
  if zipper.Write( i, sizeof( i))<>sizeof( i) then
    lastbiserror:= bewriteerror;
  i:= source.height;
  if zipper.Write( i, sizeof( i))<>sizeof( i) then
    lastbiserror:= bewriteerror;
  i:= source.BitsPerPixel;
  if zipper.Write( i, sizeof( i))<>sizeof( i) then
    lastbiserror:= bewriteerror;

  i:= source.DIBPalEntryCount;
  if zipper.Write( i, sizeof( i))<>sizeof( i) then
    lastbiserror:= bewriteerror;
  for i:= 0 to i-1 do
    begin
      bpp:= source.DIBPalEntries[ i];
      if zipper.Write( bpp, sizeof( bpp))<>sizeof( bpp) then
       lastbiserror:= bewriteerror;
    end;

  case source.PixelFormat of
    pf1bit: bpp:= 1;
    pf4bit: bpp:= 4;
    pf8bit: bpp:= 8;
    pf15bit: bpp:= 15;
    pf16bit: bpp:= 16;
    pf24bit: bpp:= 24;
    else bpp:= 32;
  end;

  // если изображение не монохромное, сначала разобьем его на битовые плоскости
  for i:= 0 to bpp-1 do
    begin
      tmpbitmap[ i]:= newbitmap( source.Width, source.Height);
      tmpbitmap[ i].PixelFormat:= pf1bit;
      patblt( tmpbitmap[ i].canvas.Handle, 0, 0, tmpbitmap[ i].Width, tmpbitmap[ i].Height, blackness);
      getmask( source, tmpbitmap[ i], i);
//      tmpbitmap[ i].SaveToFile( int2str( i)+'.bmp');
    end;

  data:= newbitsex;
  m.x:= 0;
  m.y:= 0;
  n.x:= source.Width-1;
  n.y:= source.Height-1;
  for i:= 0 to bpp-1 do
    begin
      src:= tmpbitmap[ i];
      scanline0:= src.ScanLine[ 0];
      scanlinedelta:= integer( src.ScanLine[ 1])-integer( scanline0);
      rewind;
      if lastbiserror<>beUnknownerror then
        begin
          zipper.free;
          data.free;
          exit;
        end;
      case pack( packrect( m, n)) of
        zZero:
          begin
            addbit( true);
            addbit( false);
          end;
        zOne:
          begin
            addbit( true);
            addbit( true);
          end;
      end;
      data.savetostream( zipper);
    end;
  zipper.Free;
  data.free;
  lastbiserror:= beOk;
end;

function BISdecompressfromstream;
var
  w, h, i, j, bpp: integer;
  m, n: tsmallpoint;
  unzipper: pstream;
begin
  lastbiserror:= beUnknownError;
  result:= nil;
  if not NewZLibDStream( unZipper, srcstream, nil) then exit;

  data:= newbitsex;
  bitindex:= 0;

  if unzipper.Read( w, sizeof( w))<>sizeof( w) then
    lastbiserror:= bereaderror;
  if unzipper.Read( h, sizeof( h))<>sizeof( h) then
    lastbiserror:= bereaderror;
  if unzipper.Read( bpp, sizeof( bpp))<>sizeof( bpp) then
    lastbiserror:= bereaderror;
  m.x:= 0;
  m.y:= 0;
  n.x:= W-1;
  n.y:= H-1;

  src:= newdibbitmap( w, h, pf1bit);
  result:= newbitmap( w, h);
  case bpp of
    1: result.PixelFormat:= pf1bit;
    4: result.PixelFormat:= pf4bit;
    8: result.PixelFormat:= pf8bit;
    15: result.PixelFormat:= pf15bit;
    16: result.PixelFormat:= pf16bit;
    24: result.PixelFormat:= pf24bit;
    32: result.PixelFormat:= pf32bit;
  end;

  if unzipper.read( i, sizeof( i))<>sizeof( i) then
    lastbiserror:= bereaderror;
  for i:= 0 to i-1 do
    begin
      if unzipper.read( j, sizeof( j))<>sizeof( j) then
        lastbiserror:= bereaderror;
      result.DIBPalEntries[ i]:= j;
    end;

  patblt( result.canvas.Handle, 0, 0, result.Width, result.Height, blackness);

  for i:= 0 to bpp-1 do
    begin
      data.LoadFromStream( unzipper);
      rewind;
      patblt( src.canvas.Handle, 0, 0, result.Width, result.Height, blackness);
      if lastbiserror<>beUnknownerror then
        begin
          unzipper.free;
          src.free;
          data.free;
          exit;
        end;
      unpack( packrect( m, n));
      scanline0:= src.ScanLine[ 0];
      scanlinedelta:= integer( src.ScanLine[ 1])-integer( scanline0);
//      src.SaveToFile( int2str( i)+'.bmp');
      putmask( src, result, i);
    end;
  unzipper.Free;
  src.free;
  data.free;
  lastbiserror:= beOk;
end;

procedure BISCompressToFile;
var
  strm: pstream;
  i: cardinal;
begin
  lastbiserror:= beUnknownError;
  strm:= newwritefilestream( dstfilename);
  if strm=nil then exit;
  i:= bismagic;
  if strm.Write( i, sizeof( i))<>sizeof( i) then
    begin
      lastbisError:= bewriteerror;
      strm.Free;
      exit;
    end;
  biscompresstostream( source, strm);
  strm.Free;
  lastbiserror:= beOk;
end;

function BISDecompressFromFile;
var
  strm: pstream;
  i: cardinal;
begin
  lastbiserror:= beUnknownError;
  result:= nil;
  strm:= newreadfilestream( sourcefilename);
  if strm=nil then exit;
  if strm.Read( i, sizeof( i))<>sizeof( i) then
    begin
      lastbisError:= bereaderror;
      strm.Free;
      exit;
    end;
  if i=bismagic then
    result:= bisdecompressfromstream( strm)
  else
    begin
      lastbiserror:= beWrongFileFormat;
      strm.free;
      exit;
    end;
  strm.free;
  lastbiserror:= beOk;
end;

end.

