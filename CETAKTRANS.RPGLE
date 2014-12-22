HDECEDIT(*JOBRUN)
FDHISZ0    IF   E           k DISK
FDBALZ0    IF   E           k DISK
FDappZ0    IF   E           k DISK
FDTRCDZ0   IF   E           k DISK
FDTRCDZ1   IF   E           k DISK
FDDESCZ0   IF   E           k DISK
FGLDCRTZ0  IF   E           k DISK
FCXREFZ0   IF   E           k DISK
FCADDRZ0   IF   E           k DISK
FXBKNAMz0  IF   E           k DISK
FPRINT4    O    E             printer oflind(*in90)
Finquiryku CF   E             WORKSTn
Dstr              S              2A
Dend              S              2A
Ds                S              1A
De                S              1A
Dbulans           S             10A   dim(12) ctdata perrcd(1)
Dstrmonth         S             10A
Dendmonth         S             10A
Dhariini          S             10A
DCabangs          S              5A
DCabang           S              5u 0
DtempRek          S             19p 0
DIsoAwal          S              8A
DIsoAkhir         S              8A
DIntAwal          S              8P 0
DIntAkhir         S              8P 0
DAwalTemp         S             15P 0
DTglSistem        S              8p 0
C                   DoW       Not *In03=*On
C                   ExFmt     CetakHist
C                   If        *In15 = *on
 /Free
  DB = 0;
  CR = 0;
  TotalDB = 0;
  TotalCR = 0;
  IsoAwal = %char(%date(tglawal:*EUR0):*ISO0);
  IsoAkhir = %char(%date(tglakhir:*EUR0):*ISO0);
  IntAwal = %Int(IsoAwal);
  IntAkhir = %int(IsoAkhir);
  hariini = %char(%date():*EUR0);
  Select;
  When %Date(tglawal:*EUR0) > %date(tglakhir:*EUR0);
   Message = 'Tanggal Awal Harus Lebih Kecil ';
  When %Date(tglakhir:*EUR0) > %Date(hariini:*EUR0);
 Message = 'Tanggal Akhir Lebih Besar dr Hari ini';
  Other;
 Message = '';
 EndSL;
 CabangS= %subst(%char(DHACCT):1:3);
 Cabang = %int(CabangS);
 Chain Cabang XBKNAMZ0;
 if %found(xbknamZ0);
  BRNAME = XNNAME;
  Endif;
  Rekening = %subst(%char(dhacct):1:3)+'-'+%subst(%char(dhacct):4:2)+'-'+
              %subst(%char(dhacct):6:5)+'-'+%subst(%char(dhacct):11:1);
/End-Free
** Awal Rutin Pengecekan Message
C                   If        Message <> ''
C                   Eval      *In54 = *On
C                   ExFmt     cetakhist
C                   Eval      *In54 = *off
C                   Eval      *In15 = *Off
C                   If        *In03 = *ON
C                   Iter
C                   EndIF
C                   Else
** Akhir dari Rutin Pengecekan Message
C                   eval      str = %subst(tglawal:3:2)
C                   eval      end = %subst(tglakhir:3:2)
 /Free
  If %DEC(str:2:0) < 10;
  evalr s=%SUBST(str:%check('0':str));
  strmonth = bulans(%dec(s:1:0));
  endIf;
  if %Dec(end:2:0) < 10;
  evalr e=%SUBST(end:%check('0':end));
  endmonth = bulans(%dec(e:1:0));
  EndIf;
  If %Dec(str:2:0) > 10 Or %Dec(end:2:0) > 10;
   strmonth = bulans(%dec(str:2:0));
   endmonth = bulans(%dec(end:2:0));
  EndIF;
 /End-Free
C                   Eval      awal = %subst(tglawal:1:2) + ' ' +
C                             %trim(strmonth) + ' ' +
C                             %subst(tglawal:5:4) + ' ' + 'S/D' + ''
C                   Eval      akhir= %subst(tglakhir:1:2) + ' ' +
C                             %trim(endmonth) + ' ' +
C                             %subst(tglakhir:5:4)
C                   Write     Periode
C                   Write     nfoRek
 /Free
  Chain Dhacct DBALZ0;
  if %found(DBALZ0);
   Nama = DMSNAM;
   Write Name;
  EndIF;
  Chain DMCCYC GLDCRTZ0;
  if %found(GLDCRTZ0);
   MataUang = GFFCDS;
  EndIf;
  TempRek = DHACCT;
  Chain TempRek CXREFZ0;
  if %found(CXREFZ0);
   Chain CXNAMK CADDRZ0;
    If %found(CADDRZ0);
     Alamat = CAADD1;
    EndIf;
   EndIf;
   Write CurAddr;
   Write PageNb;
   Akhir2 = Akhir;
   Write Last;
   ExSr alwal;
   ExSr Mutation;
   Write Totals;
   Write Balance;
  /End-Free
C                   EndIf
C                   EndIf
C                   EndDo
C                   Eval      *In15 = *Off
C                   Eval      *InLr = *On

 /Free
  // ---* Subrutin Untuk Menghasilkan Saldo Awal * --- //
  BegSr alwal;
  Redaksi = 'Saldo Awal ............:';
  // Mengambil Current Balance dan Mengambil Tanggal Sistem
  Chain Dhacct DbalZ0;
  Chain (Cabang:DMAPCD:DMCCYC) DAPPZ0;
   if %Found(DAPPZ0);
    TglSistem = DADTCP;
   EndIF;
  // Kalau Tanggal Akhir Lebih Kecil Dari Tanggal Sistem
  If %date(IntAkhir) < %date(TglSistem);
   AwalTemp = DMBLCR;
   SetLL Dhacct DhisZ0;
   Read Dhisz0;
  // Looping mencari saldo current balance per tanggal akhir
   DoW Not %Eof(dhisZ0);
    If DHPSDT > IntAkhir And DHPSDT <= TglSistem;
     if DHDBCR = 'D';
      AwalTemp += DhAmt;
     EndIf;
     If DHDBCR = 'C';
      AwalTemp -= DhAmt;
     EndIF;
    EndIf;
    Reade DHACCT DhisZ0;
   EndDo;
   // Menjadikan Saldo Awal sebagai Current Balance, Starting Poin
   SaldoAwal = AwalTemp;
   SetLL Dhacct Dhisz0;
   Read DhisZ0;
   DoW not %Eof(dhisZ0);
    If DHPSDT >= IntAwal And  DHPSDT <= IntAkhir;
     if DHDBCR = 'D';
      SaldoAwal += DHAMT;
     EndIF;
     If DHDBCR = 'C';
      SaldoAwal -= DHAMT;
     EndIf;
    EndIF;
    Reade Dhacct DhisZ0;
   EndDo;
   Write SalWal;
   // Kalau Tanggal akhir sama dengan tanggal sistem, lakukan looping
   // Dibawah ini
  Else;
   SaldoAwal = DMBLCR;
   SetLL Dhacct dhisZ0;
   Read DhisZ0;
   DoW not %Eof(dhisZ0);
   If DHPSDT >= IntAwal And  DHPSDT <= IntAkhir;
    if DHDBCR = 'D';
     SaldoAwal += DHAMT;
    EndIF;
    If DHDBCR = 'C';
     SaldoAwal -= DHAMT;
    EndIf;
   EndIF;
   Reade Dhacct DhisZ0;
  EndDo;
   Write SalWal;
  EndIF;
  EndSr;
  // * --- Akhir Dari Subrutin Untuk Menghasilkan Saldo Awal --- * //
  
  
  // * --- Awal Dari Subruti Untuk Menghasilkan Mutasi Trans --- * //
 BegSr Mutation;
 // Nilai Mutasi diambil dari saldoawal
 Mutasi = SaldoAwal;
 // Untuk Redaksi Offline Indicator
  Redaksi = 'Saldo Pindahan ........:';
  // Posisi Di Record Dengan Nomor Rekening Yang Dicari
  SetLL DHACCT Dhisz0;
  // Baca File DhisZ0
  Read Dhisz0;
  // Selama Belum End Of File Dhisz0
  DoW Not %EOF(Dhisz0);
   // Jika Masih didalam range tanggal yang dicari
   If DhPsdt >= IntAwal and DhPsdt <= IntAkhir;
    // Tanggal dan Kodtran Diisi Dari Nilai Di Dhisz0
   Tanggal = %Char(%Date(%char(DHPSDT):*ISO0):*DMY);
   Kodtran = DHTXCD;
   // Kalau Transaksinya adalah debit
   If DhDbCr = 'D';
    Debit = Dhamt;
    TotalDB += Debit;
    Kredit = *Zeros;
    Mutasi -= DhAmt;
    DB += 1;
   EndIF;
   // Kalau Transaksinya adalah kredit
   If DhDbCr = 'C';
    Kredit = Dhamt;
    TotalCr += Kredit;
    Debit = *Zeros;
    Mutasi += DhAmt;
    CR += 1;
   EndIF;
   // Kalau Sequence Transaksi Untuk Deskripsinya adalah 0
   If DHDES# = 0;
    // Kodtran Bukan Reversal
    If %int(%SubSt(%Char(DHTXCD):1:1)) < 4;
     Chain (DHTAPC:DHTXCD:DHDBCR) DTRCDZ0;
      Deskripsi = %trim(DFTRDS);
    Else;
     // Kodtran Reversal
     Chain (DHTAPC:DHTXCD:DHDBCR) DTRCDZ1;
      Deskripsi = %trim(DFTRDS);
    EndIF;
   Else;
    // DHDES# tidak sama dengan 0
    Chain (DHACCT:DHDES#) DDESCZ0;
     Deskripsi = %trim(DDDESC);
   EndIF;
  // Tulis Mutasi
  Write Mutate;
  // Jika End Of Page Dari Printer File
  If *In90 = *On;
   SaldoAwal = Mutasi;
   Write Periode;
   Write NfoRek;
   Write Name;
   Write CurAddr;
   Write PageNb;
   Write Last;
   Write SalWal;
   *In90 = *Off;
  EndIF;
  // Mencari Rekening Selanjutnya Di Range Tanggal Tersebut
  EndIF;
   ReadE Dhacct Dhisz0;
  EndDo;
  Efektif = Mutasi;
  EndSr;
  // * --- Akhir 7ari Subruti Untuk Menghasilkan Mutasi Trans --- * //  

 /End-Free
 
 **CTDATA bulans
 Januari
 Februari
 Maret
 April
 Mei
 Juni
 Juli
 Agustus
 September
 Oktober
 November
 Desember
 
  
