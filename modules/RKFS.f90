SUBROUTINE RKFS(F, NEQN, Y, T, TOUT, RELERR, ABSERR, IFLAG, &
   YP, H, F1, F2, F3, F4, F5, SAVRE, SAVAE, &
   NFE, KOP, INIT, JFLAG, KFLAG)
   !
   !     METOД PУHГE-KУTTA-ФEЛЬБEPГA ЧETBEPTOГO-ПЯTOГO ПOPЯДKA
   !     RKFS ИHTEГPИPУET CИCTEMУ OБЫKHOBEHHЫX ДИФФE-
   !     PEHЦИAЛЬHЫX УPABHEHИЙ ПEPBOГO ПOPЯДKA(CM. KOM-
   !     MEHTAPИЙ K RKF45). MACCИBЫ YP,F1,F2,F3,F4 И F5
   !     (PAЗMEPHOCTИ ПO KPAЙHEЙ MEPE NEQN) И ПEPEMEH-
   !     HЫE H,SAVRE,SAVAE,NFE,KOP,INIT,JFLAG И KFLAG
   !     ИCПOЛЬЗУЮTCЯ BHУTPИ ПPOГPAMMЫ И BЫHECEHЫ B
   !     CПИCOK BЫЗOBA,ЧTOБЫ COXPAHИTЬ ИX OПPEДEЛEH-
   !     HOCTЬ ПPИ ПOBTOPHOM OБPAЩEHИИ.ПOЭTOMУ ИX ЗHA-
   !     ЧEHИЯ HE ДOЛЖHЫ  ИЗMEHЯTЬCЯ  ПOЛЬЗOBATEЛEM.
   !     BOЗMOЖHЫЙ ИHTEPEC ПPEДCTABЛЯЮT ПAPAMETPЫ
   !     YP  -ПPOИЗBOДHAЯ BEKTOPA PEШEHИЯ B TOЧKE T
   !     H   -ПPEДПOЛAГAEMЫЙ PAЗMEP ШAГA ДЛЯ OЧEPEДHOГO ЭTAПA
   !     NFE -CЧETЧИK ЧИCЛA BЫЧИCЛEHИЙ ФУHKЦИИ
   !
   LOGICAL HFAILD, OUTPUT
   !
   INTEGER NEQN, IFLAG, NFE, KOP, INIT, JFLAG, KFLAG
   REAL Y(NEQN), T, TOUT, RELERR, ABSERR, H, YP(NEQN), &
      F1(NEQN), F2(NEQN), F3(NEQN), F4(NEQN), F5(NEQN), &
      SAVRE, SAVAE
   !
   EXTERNAL F
   !
   REAL A, AE, DT, EE, EEOET, ESTTOL, ET, HMIN, REMIN, &
      RER, S, SCALE, TOL, TOLN, U26, EPSP1, EPS, YPK
   !
   INTEGER K, MAXNFE, MFLAG
   !
   REAL AMAX1, AMIN1
   SAVE
   !
   !     REMIN-ЭTO MИHИMAЛЬHOE ДOПУCTИMOE ЗHAЧEHИE ДЛЯ
   !     RELERR.ПOПЫTKИ ПOЛУЧИTЬ ПO ЭTOЙ ПOДПPOГPAMME
   !     БOЛEE BЫCOKУЮ TOЧHOCTЬ OБЫЧHO CTOЯT OЧEHЬ
   !     ДOPOГO И ЗAЧACTУЮ БEЗУCПEШHЫ.
   !
   DATA REMIN/1.E-12/
   !
   !     CTOИMOCTЬ CЧETA KOHTPOЛИPУETCЯ TPEБOBAHИEM,
   !     ЧTOБЫ KOЛИЧECTBO BЫЧИCЛEHИЙ ФУHKЦИИ БЫЛO OГ-
   !     PAHИЧEHO BEЛИЧИHOЙ,ПPИБЛИЗИTEЛЬHO PABHOЙ ЗHA-
   !     ЧEHИЮ ПAPAMETPA MAXNFE.ПPИHЯTOE ЗДECЬ ЗHAЧE-
   !     HИE ПPИMEPHO COOTBETCTBУET 500 ШAГAM.
   !
   DATA MAXNFE/3000/
   !
   !     ПPOBEPИTЬ BXOДHЫE ПAPAMETPЫ
   !
   IF(NEQN<1)GO TO 10
   IF((RELERR<0.0).OR.(ABSERR<0.0))GO TO 10
   MFLAG = IABS(IFLAG)
   IF((MFLAG==0).OR.(MFLAG>8))GO TO 10
   IF(MFLAG/=1)GO TO 20
   !
   !     ПEPBЫЙ BЫЗOB,BЫЧИCЛИTЬ MAШИHHOE ЭПCИЛOH
   !
   EPS = 1.0
   5 EPS = EPS / 2.0
   EPSP1 = EPS + 1.
   IF(EPSP1>1.)GO TO 5
   U26 = 26. * EPS
   GO TO 50
   !
   !     OШИБKA BXOДHOЙ ИHФOPMAЦИИ
   !
   10 IFLAG = 8
   RETURN
   !
   !     ПPOBEPИTЬ BOЗMOЖHOCTИ ПPOДOЛЖEHИЯ
   !
   20 IF((T==TOUT).AND.(KFLAG/=3))GO TO 10
   IF(MFLAG/=2)GO TO 25
   !
   !     IFLAG=+2 ИЛИ -2
   !
   IF((KFLAG==3).OR.(INIT==0))GO TO 45
   IF(KFLAG==4)GO TO 40
   IF((KFLAG==5).AND.(ABSERR==0.0))GO TO 30
   IF((KFLAG==6).AND.(RELERR<=SAVRE).AND.&
      (ABSERR<=SAVAE))GO TO 30
   GO TO 50
   !
   !     IFLAG=3,4,5,6,7 ИЛИ 8
   !
   25 IF(IFLAG==3)GO TO 45
   IF(IFLAG==4)GO TO 40
   IF((IFLAG==5).AND.(ABSERR>0.0))GO TO 45
   !
   !     ИHTEГPИPOBAHИE HEЛЬЗЯ ПPOДOЛЖATЬ,ПOCKOЛЬKУ ПOЛЬ-
   !     ЗOBATEЛЬ HE BЫПOЛHИЛ ИHCTPУKЦИЙ,COOTBETCTBУЮЩИX
   !     ЗHAЧEHИЯM IFLAG=5,6,7 ИЛИ 8
   !

   30 STOP
   !
   !     ПEPEOПPEДEЛИTЬ CЧETЧИK ЧИCЛA BЫЧИCЛEHИЙ ФУHKЦИИ
   !
   40 NFE = 0
   IF(MFLAG==2)GO TO 50
   !
   !     ПEPEOПPEДEЛИTЬ ЗHAЧEHИE FLAG,УCTAHOBЛEHHOE
   !     ПPИ ПPEДЫДУЩEM OБPAЩEHИИ
   !
   45 IFLAG = JFLAG
   IF(KFLAG==3)MFLAG = IABS(IFLAG)
   !
   !     COXPAHИTЬ BXOДHOE ЗHAЧEHИE IFLAG И УCTAHOBИTЬ
   !     ЗHAЧEHИE FLAG, COOTBETCTBУЮЩEE ПPOДOЛЖEHИЮ,
   !     ДЛЯ БУДУЩEЙ ПPOBEPKИ
   !
   50 JFLAG = IFLAG
   KFLAG = 0
   !
   !     COXPAHИTЬ ЗHAЧEHИЯ RELERR И ABSERR ДЛЯ BXOДHOЙ
   !     ПPOBEPKИ ПPИ ПOCЛEДУЮЩИX OБPAЩEHИЯX
   !
   SAVRE = RELERR
   SAVAE = ABSERR
   !
   !     УCTAHOBИTЬ ЗHAЧEHИE ГPAHИЦЫ ДЛЯ OTHOCИTEЛЬ-
   !     HOЙ ПOГPEШHOCTИ,PABHOE KAK MИHИMУM 2*EPS+
   !     REMIN,ЧTOБЫ ИЗБEЖATЬ TPУДHOCTEЙ,CBЯЗAHHЫX
   !     C TPEБOBAHИEM HEДOCTИЖИMOЙ TOЧHOCTИ
   !
   RER = 2. * EPS + REMIN
   IF(RELERR>=RER)GO TO 55
   !
   !     ЗAДAHHAЯ ГPAHИЦA OTHOCИTEЛЬHOЙ ПOГPEШHOCTИ
   !     CЛИШKOM MAЛA
   !
   RELERR = RER
   IFLAG = 3
   KFLAG = 3
   RETURN
   !
   55 DT = TOUT - T
   !
   IF(MFLAG==1)GO TO 60
   IF(INIT==0)GO TO 65
   GO TO 80
   !
   !     ПPИCBOEHИE HAЧAЛЬHЫX ЗHAЧEHИЙ (ИHИЦИИPOBA-
   !             HИE)-УCTAHOBИTЬ ЗHAЧEHИE УKAЗATEЛЯ
   !             OKOHЧAHИЯ ИHИЦИИPOBAHИЯ,INIT
   !             УCTAHOBИTЬ ЗHAЧEHИE УKAЗATEЛЯ CЛИШ-
   !             KOM БOЛЬШOГO ЗATPEБOBAHHOГO ЧИCЛA BЫ-
   !             XOДHЫX TOЧEK,KOP
   !             BЫЧИCЛИTЬ HAЧAЛЬHЫE ПPOИЗBOДHЫE
   !             УCTAHOBИTЬ ЗHAЧEHИE CЧETЧИKA ЧИCЛA
   !             BЫЧИCЛEHИЙ ФУHKЦИИ,NFE
   !             OЦEHИTЬ HAЧEЛЬHУЮ BEЛИЧИHУ ШAГA
   !
   60 INIT = 0
   KOP = 0
   !
   A = T
   CALL F(A, Y, YP)
   NFE = 1
   IF(T/=TOUT)GO TO 65
   IFLAG = 2
   RETURN
   !
   65 INIT = 1
   H = ABS(DT)
   TOLN = 0.
   DO K = 1, NEQN
      TOL = RELERR * ABS(Y(K)) + ABSERR
      IF(TOL<=0)GO TO 70
      TOLN = TOL
      YPK = ABS(YP(K))
      IF(YPK * H**5>TOL)H = (TOL / YPK)**0.2
   70 CONTINUE
   end do
   IF(TOLN<=0.0)H = 0.0
   H = AMAX1(H, U26 * AMAX1(ABS(T), ABS(DT)))
   JFLAG = ISIGN(2, IFLAG)
   !
   !     ПPИCBOИTЬ BEЛИЧИHE ШAГA ЗHAK,COOTBETCTBУЮЩИЙ
   !     ИHTEГPИPOBAHИЮ B HAПPABЛEHИИ OT T K TOUT
   !
   80 H = SIGN(H, DT)
   !
   !     ПPOBEPKA, HACKOЛЬKO CEPЬEЗHO BЛИЯHИE HA RKF45
   !     CЛИШKOM БOЛЬШOГO ЗATPEБOBAHHOГO ЧИCЛA BЫXOД-
   !     HЫX TOЧEK
   !
   IF(ABS(H)>=2.0 * ABS(DT))KOP = KOP + 1
   IF(KOP/=100) GO TO 85
   !
   !     ЧPEЗMEPHAЯ ЧACTOTA BЫXOДOB
   !
   KOP = 0
   IFLAG = 7
   RETURN
   !
   85 IF(ABS(DT)>U26 * ABS(T)) GO TO 95
   !
   !     ECЛИ OЧEHЬ БЛИЗKO K TOЧKE BЫXOДA,ПPOЭKCTPAПO-
   !     ЛИPOBATЬ И BEPHУTЬCЯ ПO MECTУ BЫЗOBA
   !
   DO K = 1, NEQN
   Y(K) = Y(K) + DT * YP(K)
   end do
   A = TOUT
   CALL F(A, Y, YP)
   NFE = NFE + 1
   GO TO 300
   !
   !     ПPИCBOИTЬ HAЧAЛЬHOE ЗHAЧEHИE ИHДИKATOPУ TOЧKИ
   !     BЫXOДA
   !
   95 OUTPUT = .FALSE.
   !
   !     ЧTOБЫ ИЗБEЖATЬ HEOПPABДAHHOГO MAШИHHOГO HУЛЯ
   !     ПPИ BЫЧИCЛEHИИ ФУHKЦИИ OT ГPAHИЦ ПOГPEШHO-
   !     CTEЙ,ПPOMACШTAБИPOBATЬ ЭTИ ГPAHИЦЫ
   !
   SCALE = 2. / RELERR
   AE = SCALE * ABSERR
   !
   !     ПOШAГOBOE ИHTEГPИPOBAHИE
   !
   100 HFAILD = .FALSE.
   !
   !     УCTAHOBИTЬ HAИMEHЬШУЮ ДOПУCTИMУЮ BEЛИЧИHУ ШAГA
   !
   HMIN = U26 * ABS(T)
   !
   !     ИCПPABИTЬ ПPИ HEOБXOДИMOCTИ BEЛИЧИHУ ШAГA,
   !     ЧTOБЫ ДOCTИГHУTЬ TOЧKИ BЫXOДA. PACCЧИTATЬ HA
   !     ДBA ШAГA BПEPEД,ЧTOБЫ ИЗБEЖATЬ CЛИШKOM PEЗKИX
   !     ИЗMEHEHИЙ B BEЛИЧИHE ШAГA И TEM CAMЫM УMEHЬ-
   !     ШИTЬ BЛИЯHИE BЫXOДHЫX TOЧEK HA ПPOГPAMMУ.
   !
   DT = TOUT - T
   IF(ABS(DT)>=2. * ABS(H))GO TO 200
   IF(ABS(DT)>ABS(H))GO TO 150
   !
   !     CЛEДУЮЩИЙ УCПEШHЫЙ ШAГ ЗABEPШИT ИHTEГPИPO-
   !     BAHИE ДO УKAЗAHHOЙ TOЧKИ BЫXOДA
   !
   OUTPUT = .TRUE.
   H = DT
   GO TO 200
   !
   150 H = 0.5 * DT
   !
   !
   !
   !     BHУTPEHHИЙ OДHOШAГOBЫЙ ИHTEГPATOP
   !
   !     ГPAHИЦЫ ПOГPEШHOCTEЙ БЫЛИ ПPOMACШTAБИPOBAHЫ,
   !     ЧTOБЫ ИЗБEЖATЬ HEOПPABДAHHOГO MAШИHHOГO HУЛЯ
   !     ПPИ BЫЧИCЛEHИИ ФУHKЦИИ OT HИX.
   !     ЧTOБЫ ИЗБEЖATЬ OБPAЩEHИЯ B HУЛЬ ЗHAMEHATEЛЯ
   !     B TECTE,OTHOCИTEЛЬHAЯ OШИБKA ИЗMEPЯETCЯ  ПO
   !     OTHOШEHИЮ K CPEДHEMУ  ИЗ BEЛИЧИH PEШEHИЯ
   !     B HAЧAЛE И KOHЦE ШAГA.
   !     B ФOPMУЛE,OЦEHИBAЮЩEЙ OШИБKУ,ПPOИЗBEДEHA
   !     ГPУППИPOBKA CЛAГAEMЫX,УMEHЬШAЮЩAЯ ПOTEPЮ
   !     BEPHЫX ЗHAKOB.
   !     ЧTOБЫ PAЗЛИЧATЬ MEЖДУ COБOЙ PAЗHЫE APГУMEHTЫ,
   !     ДЛЯ H HE ДOПУCKAЮTCЯ ЗHAЧEHИЯ,MEHЬШИE УMHO-
   !     ЖEHHOЙ HA 26 OШИБKИ OKPУГЛEHИЯ B T.
   !     BBEДEHЫ ПPAKTИЧECKИE OГPAHИЧEHИЯ HA CKOPOCTЬ
   !     ИЗMEHEHИЯ BEЛИЧИHЫ ШAГA,ЧTOБЫ CГЛAДИTЬ ПPO-
   !     ЦECC BЫБOPA ЭTOЙ BEЛИЧИHЫ И ИЗБEЖATЬ ЧPEЗMEP-
   !     HOГO EE PAЗБPOCA B ЗAДAЧAX C HAPУШEHИEM HEПPE-
   !     PЫBHOCTИ.
   !     ИЗ ПPEДOCTOPOЖHOCTИ ПPOГPAMMA БEPET 9/10 OT TOЙ
   !     BEЛИЧИHЫ ШAГA,KOTOPAЯ HУЖHA ПO EE OЦEHKE.
   !     ECЛИHA ДAHHOM ШAГE БЫЛA HEУДAЧHAЯ ПOПЫTKA
   !     TO ПPИ ПЛAHИPOBAHИИ CЛEДУЮЩEГO УBEЛИЧEHИE
   !     ДЛИHЫ ШAГA HE ДOПУCKAETCЯ. ЭTO ПOBЫШAET ЭФФEK-
   !     TИBHOCTЬ ПPOГPAMMЫ ДЛЯ ЗAДAЧ C PAЗPЫBAMИ И
   !     B OБЩEM CЛУЧAE,ПOCKOЛЬKУ ИCПOЛЬЗУETCЯ ЛOKAЛЬ-
   !     HAЯ ЭKCTPAПOЛЯЦИЯ И ДOПOЛHИTEЛЬHAЯ ПPEДOCTO-
   !     POЖHOCTЬ KAЖETCЯ OПPABДAHHOЙ.
   !
   !
   !     ПPOBEPИTЬ ЧИCЛO BЫЧИCЛEHИЙ ПPOИЗBOДHЫX.ECЛИ
   !     OHO HE ПPEBЫШAET УCTAHOBЛEHHOГO ПPEДEЛA,ПO-
   !     ПPOБOBATЬ ПPOДOЛЖИTЬ ИHTEГPИPOBAHИE C T ДO T+H
   !
   200 IF(NFE<=MAXNFE)GO TO 220
   !
   !     CЛИШKOM БOЛЬШAЯ PAБOTA
   !
   IFLAG = 4
   KFLAG = 4
   RETURN
   !
   !     ПPOДOЛЖИTЬ ПPИБЛИЖEHHOE PEШEHИE HA OДИH ШAГ ДЛИHЫ H
   !
   220 CALL FEHL(F, NEQN, Y, T, H, YP, F1, F2, F3, F4, F5, F1)
   NFE = NFE + 5
   !
   !     BЫЧИCЛИTЬ И CPABHИTЬ ДOПУCTИMЫE ГPAHИЦЫ И
   !     OЦEHKИ ЛOKAЛЬHOЙ OШИБ,A ЗATEM CHЯTЬ MACШTA-
   !     БИPOBAHИE ГPAHИЦ.ЗAMETЬTE,ЧTO OTHOCИTEЛЬHAЯ
   !     OШИБKA ИЗMEPЯETCЯ ПO OTHOШEHИЮ K CPEДHEMУ ИЗ
   !     BEЛИЧИH PEШEHИЯ B HAЧAЛE И KOHЦE ШAГA.
   !
   EEOET = 0.
   DO K = 1, NEQN
      ET = ABS(Y(K)) + ABS(F1(K)) + AE
      IF(ET>0.)GO TO 240
      !
      !     HEПPABИЛЬHAЯ ГPAHИЦA ПOГPEШHOCTИ
      !
      IFLAG = 5
      KFLAG = 5
      RETURN
      !
      240 EE = ABS((-2090. * YP(K) + (21970. * F3(K) - 15048. * F4(K)))&
         + (22528. * F2(K) - 27360. * F5(K)))
   EEOET = AMAX1(EEOET, EE / ET)
   end do
   !
   ESTTOL = ABS(H) * EEOET * SCALE / 752400.
   !
   IF(ESTTOL<=1.0)GO TO 260
   !
   !
   !     HEУДAЧHЫЙ ШAГ
   !            УMEHЬШИTЬ BEЛИЧИHУ ШAГA И CHOBA ПO-
   !            ПPOБOBATЬ
   !            УMEHЬШEHИE OГPAHИЧИBAETCЯ CHИЗУ MHO-
   !            ЖИTEЛEM 1/10
   !
   HFAILD = .TRUE.
   OUTPUT = .FALSE.
   S = 0.1
   IF(ESTTOL<59049.)S = 0.9 / ESTTOL**0.2
   H = S * H
   IF(ABS(H)>HMIN)GO TO 200
   !
   !     ЗAДAHHAЯ ГPAHИЦA OШИБKИ HEДOCTИЖИMA ДAЖE ПPИ
   !     HAИMEHЬШEЙ ДOПУCTИMOЙ BEЛИЧИHE ШAГA
   !
   IFLAG = 6
   KFLAG = 6
   RETURN
   !
   !
   !     УCПEШHЫЙ ШAГ
   !            ПOMECTИTЬ B MACCИB Y PEШEHИE B TOЧKE
   !            T+H И BЫЧИCЛИTЬ ПPOИЗBOДHЫE B ЭTOЙ
   !            TOЧKE
   !
   260 T = T + H
   DO K = 1, NEQN
   Y(K) = F1(K)
   end do
   A = T
   CALL F(A, Y, YP)
   NFE = NFE + 1
   !
   !
   !     BЫБPATЬ BEЛИЧИHУ CЛEДУЮЩEГO ШAГA
   !     УBEЛИЧEHИE OГPAHИЧEHO MHOЖИTEЛEM 5
   !     ECЛИ HA ДAHHOM ШAГE БЫЛA HEУДAЧHAЯ
   !     ПOПЫTKA,TO ДЛЯ CЛEДУЮЩEГO HE ДOПУ-
   !     CKAETCЯ BЫБOP БOЛЬШEЙ BEЛИЧИHЫ ШAГA
   !
   S = 5.
   IF(ESTTOL>1.889568E-4)S = 0.9 / ESTTOL**0.2
   IF(HFAILD)S = AMIN1(S, 1.0)
   H = SIGN(AMAX1(S * ABS(H), HMIN), H)
   !
   !     KOHEЦ OДHOШAГOBOГO ИHTEГPATOPA
   !
   !
   !     HУЖHO ЛИ ДEЛATЬ OЧEPEДHOЙ ШAГ
   !
   IF(OUTPUT)GO TO 300
   IF(IFLAG>0)GO TO 100
   !
   !
   !     ИHTEГPИPOBAHИE УCПEШHO ЗABEPШEHO
   !
   !     PEЖИM OДHOШAГOBOГO ИHTEГPИPOBAHИЯ
   !
   IFLAG = -2
   RETURN
   !
   !     PEЖИM ИHTEГPИPOBAHИЯ HA ИHTEPBAЛE
   !
   300 T = TOUT
   IFLAG = 2
   RETURN
   !
END
