SUBROUTINE RKF45(F, NEQN, Y, T, TOUT, RELERR, ABSERR, &
   IFLAG, WORK, IWORK)
   !
   !     METOД PУHГE-KУTTA ФEЛЬБEPГA ЧETBEPTOГO-ПЯTOГO ПOPЯДKA
   !
   !     COCTABИTEЛИ ПPOГPAMMЫ-H.A.WATTS,L.F.SHAMPINE
   !                SANDIA LABORATORIES
   !              ALBUQUERQUE, NEW MEXICO
   !
   !     RKF45 ПPEДHAЗHAЧEHA ГЛABHЫM OБPAЗOM ДЛЯ PEШEHИЯ
   !     HEЖECTKИX И CЛAБO ЖECTKИX ДИФФEPEHЦИAЛЬHЫX УPABHEHИЙ,
   !     KOГДA BЫЧИCЛEHИE ПPOИЗBOДHЫX HE CЛИШKOM ДOPOГOCTOЯЩEE.
   !     RKF45, BOOБЩE ГOBOPЯ,HE CЛEДУET ИCПOЛЬЗOBATЬ
   !     ECЛИ ПOЛЬЗOBATEЛЮ TPEБУETCЯ BЫCOKAЯ TOЧHOCTЬ
   !
   !     PEЗЮME
   !
   !     ПOДПPOГPAMMA RKF45 ИHTEГPИPУET CИCTEMУ ИЗ NEQN
   !     OБЫKHOBEHHЫX ДИФФEPEHЦИAЛЬHЫX УPABHEHИЙ ПEPBOГO
   !     ПOPЯДKA CЛEДУЮЩEГO BИДA:
   !
   !            DY(I)/DT=F(T,Y(1),Y(2),...,Y(NEQN),
   !
   !     ГДE Y(I) ЗAДAHЫ B T.
   !     OБЫЧHO ПOДПPOГPAMMУ ПPИMEHЯЮT ДЛЯ ИHTEГPИPOBAHИЯ
   !     OT T ДO TOUT, OДHAKO EE MOЖHO ИCПOЛЬЗOBATЬ И KAK
   !     OДHOШAГOBЫЙ ИHTEГPATOP,ЧTOБЫ ПPOДOЛЖИTЬ PEШEHИE HA
   !     OДИH ШAГ B HAПPABЛEHИИ TOUT.HA BЫXOДE ПAPAMETPAM,
   !     ФИГУPИPУЮЩИM B CПИCKE BЫЗOBA, ПPИCBAИBAЮTCЯ ЗHAЧEHИЯ,
   !     HEOБXOДИMЫE ДЛЯ ПPOДOЛЖEHИЯ ИHTEГPИPOBAHИЯ. ПOЛЬЗO-
   !     BATEЛЮ HУЖHO ЛИШЬ EЩE PAЗ OБPATИTЬCЯ K RKF45
   !     (И,BOЗMOЖHO,OПPEДEЛИTЬ HOBOE ЗHAЧEHИE ДЛЯ TOUT).
   !     B ДEЙCTBИTEЛЬHOCTИ RKF45-ЭTO ПPOГPAMMA ИHTEPФEЙCA,
   !     KOTOPAЯ BЫЗЫBAET ПOДПPOГPAMMУ RKFS, OCУЩECTBЛЯЮЩУЮ
   !     ПPOЦECC PEШEHИЯ.RKFS B CBOЮ OЧEPEДЬ BЫЗЫBAET ПOДПPOГ-
   !     PAMMУ FEHL, KOTOPAЯ BЫЧИCЛЯET ПPИБЛИЖEHHOE PEШEHИE
   !     HA OДИH ШAГ.
   !
   !     RKF45 ИCПOЛЬЗУET METOД PУHГE-KУTTA-ФEЛЬБEPГA, OПИCAHHЫЙ
   !     B CЛEДУЮЩEЙ ПУБЛИKAЦИИ:E.FEHLBERG,LOW-ORDER CLASSICAL
   !     RUNGE-KUTTA FORMULAS WITH STEPSIZE CONTROL,NASA TR R-315
   !
   !     CTИЛЬ PAБOTЫ ПPOГPAMMЫ RKF45 ИЛЛЮCTPИPУETCЯ B CЛEДУЮЩИX
   !     ПУБЛИKAЦИЯX:L.F.SHAMPINE,H.A.WATTS,S.DAVENPORT, SOLVING
   !     NON-STIFF ORDINARY DIFFERENTIAL EQUATIONS-THE STAT OF
   !     THE ART,SANDIA LABORATORIES REPORT SAND75-0182,SIAM
   !     REVIEW,18(1976), N3,376-411.
   !
   !     ПAPAMETPЫ ПPOГPAMMЫ:
   !
   !     F       -ПOДПPOГPAMMA F(T,Y,YP) ДЛЯ BЫЧИCЛEHИЯ
   !              ПPOИЗBOДHЫX YP(I)=DY(I)/DT
   !     NEQN    -ЧИCЛO ИHTEГPИPУEMЫX УPABHEHИЙ
   !     Y(*)    -PEШEHИE B TOЧKE T
   !     T       -HEЗABИCИMAЯ ПEPEMEHHAЯ
   !     TOUT    -TOЧKA BЫXOДA,B KOTOPOЙ HУЖHO OПPEДEЛИTЬ
   !              ЗHAЧEHИE PEШEHИЯ
   !     RELERR  -ГPAHИЦA OTHOCИTEЛЬHOЙ ПOГPEШHOCTИ
   !              ДЛЯ TECTA ЛOKAЛЬHOЙ OШИБKИ.
   !     ABSERR  -ГPAHИЦA ABCOЛЮTHOЙ ПOГPEШHOCTИ
   !              ДЛЯ TECTA ЛOKAЛЬHOЙ OШИБKИ.
   !              HA KAЖДOM ШAГE ПPOГPAMMA TPEБУET BЫПOЛHEHИЯ УCЛOBИЯ
   !              ABS(LOCAL ERROR).LE.RELERR*ABS(Y)+ABSERR
   !              ДЛЯ KAЖДOЙ KOMПOHEHTЫ BEKTOPOB ЛOKAЛЬHOЙ
   !              OШИБKИ И PEШEHИЯ
   !     IFLAG   -УKAЗATEЛЬ PEЖИMA ИHTEГPИPOBAHИЯ.
   !     WORK(*) -MACCИB,COДEPЖAЩИЙ ИHФOPMAЦИЮ,BHУTPEHHЮЮ ДЛЯ RKF45,
   !              KOTOPAЯ HEOБXOДИMA ПPИ ПOCЛEДУЮЩИX BЫЗOBAX.EГO
   !              PAЗMEPHOCTЬ ДOЛЖHA БЫTЬ HE MEHЬШE 3+6*NEQN
   !     IWORK(*)-ЦEЛЫЙ MACCИB,COДEPЖAЩИЙ ИHФOPMAЦИЮ,BHУTPEHHЮЮ ДЛЯ
   !              RKF45,KOTOPAЯ HEOБXOДИMA ПPИ ПOCЛEДУЮЩИX BЫЗOBAX.
   !              EГO PAЗMEPHOCTЬ ДOЛЖHA БЫTЬ HE MEHЬШE 5.
   !
   !     ПEPBOE OБPAЩEHИE K RKF45
   !
   !     ПOЛЬЗOBATEЛЬ ДOЛЖEH ПPEДУCMOTPETЬ B CBOEЙ BЫЗЫBAЮЩEЙ
   !     ПPOГPAMME ПAMЯTЬ ДЛЯ CЛEДУЮЩИX MACCИBOB, ФИГУPИPУЮЩИX
   !     B CПИCKE BЫЗOBA- Y(NEQN), WORK(3+6*NEQN), IWORK(5);
   !     KPOME TOГO, OH ДOЛЖEH OБ'ЯBИTЬ F B OПEPATOPE EXTERNAL,
   !     ПOДГOTOBИTЬ ПOДПPOГPAMMУ F(T,Y,YP) И ПPИCBOИTЬ HAЧAЛЬ-
   !     HЫE ЗHAЧEHИЯ ПAPAMETPAM-
   !
   !     NEQN  -ЧИCЛO ИHTEГPИPУEMЫX УPABHEHИЙ (NEQN.GE.1)
   !     Y(*)  -BEKTOP HAЧAЛЬHЫX УCЛOBИЙ
   !     T     -HAЧAЛЬHAЯ TOЧKA ИHTEГPИPOBAHИЯ,
   !            T ДOЛЖHO БЫTЬ ПEPEMEHHOЙ.
   !     TOUT  -TOЧKA BЫXOДA,B KOTOPOЙ HУЖHO HAЙTИ ЗHAЧEHИE
   !            PEШEHИЯ. T=TOUT BOЗMOЖHO ЛИШЬ ПPИ ПEPBOM
   !            OБPAЩEHИИ.B ЭTOM CЛУЧAE BЫXOД ИЗ RKF45 ПPOИ-
   !            CXOДИT CO ЗHAЧEHИEM ПAPAMETPA IFLAG=2,ECЛИ
   !            MOЖHO ПPOДOЛЖATЬ ИHTEГPИPOBAHИE.
   !     RELERR-ГPAHИЦA ДЛЯ OTHOCИTEЛЬHOЙ ЛOKAЛЬHЫЙ ПOГPEШHOCTEИ.
   !     ABSERR-ГPAHИЦA ДЛЯ AБCOЛЮTHOЙ    ЛOKAЛЬHЫЙ ПOГPEШHOCTEИ.
   !            ЭTИ ГPAHИЦЫ ДOЛЖHЫ БЫTЬ HEOTPИЦATEЛЬHЫ.
   !            RELERR ДOЛЖHA БЫTЬ ПEPEMEHHOЙ,A ABSERR MOЖET
   !            БЫTЬ И KOHCTAHTOЙ.ПPOГPAMME, BOOБЩE ГOBOPЯ
   !            HE CЛEДУET ЗAДABATЬ ГPAHИЦУ ДЛЯ OTHOCИTEЛЬHOЙ
   !            OШИБKИ,MEHЬШУЮ, ЧEM ПPИMEPHO 1.E-7. ДAБЫ ИЗБEЖATЬ
   !            TPУДHOCTEЙ ,CBЯЗAHHЫX C OЧEHЬ BЫCOKИMИ ЗAПPOCAMИ
   !            K TOЧHOCTИ, ПPOГPAMMA TPEБУET,ЧTOБЫ RELERR
   !            БЫЛA БOЛЬШE, ЧEM HEKOTOPЫЙ ПAPAMETP OTHOCИTEЛЬHOЙ
   !            OШИБKИ,BЫЧИCЛЯEMЫЙ BHУTPИ EE И ЗABИCЯЩИЙ OT
   !            MAШИHЫ.B ЧACTHOCTИ,HE PAЗPEШAETCЯ ЗAДAHИE TOЛЬKO
   !            AБCOЛЮTHOЙ OШИБKИ.ECЛИ ЖE ЗAДAHO ЗHAЧEHИE RELERR,
   !            MEHЬШEE ДOПУCTИMOГO, TO RKF45 УBEЛИЧИBAET RELERR
   !            HAДЛEЖAЩИM OБPAЗOM И BOЗBPAЩAET УПPABЛEHИE ПOЛЬ-
   !            ЗOBATEЛЮ, ПPEЖДE ЧEM ПPOДOЛЖATЬ ИHTEГPИPOBAHИE.
   !     IFLAG-+1,-1.ЭTO УKAЗATEЛЬ HACTPOЙKИ ПPOГPAMMЫ ДЛЯ KAЖДOЙ
   !            HOBOЙ ЗAДAЧИ. HOPMAЛЬHOE BXOДHOE ЗHAЧEHИE PABHO+1.
   !            ПOЛЬЗOBATEЛЬ ДOЛЖEH ЗAДABATЬ IFLAG=-1 ЛИШЬ B TOM
   !            CЛУЧAE,  KOГДA HEOБXOДИMO УПPABЛEHИE OДHOШAГOBЫM
   !            ИHTEГPATOPOM.B ЭTOM CЛУЧAE RKF45 ПЫTAETCЯ ПPOДOЛЖИTЬ
   !            PEШEHИE HA OДИH ШAГ B HAПPABЛEHИИ TOUT ПPИ KAЖДOM
   !            OЧEPEДHOM BЫЗOBE. ПOCKOЛЬKУ ЭTOT PEЖИM PAБOTЫ
   !            BECЬMA HEЭKOHOMИЧEH, EГO CЛEДУET ПPИMEHЯTЬ
   !            ЛИШЬ B CЛУЧAE KPAЙHEЙ HEOБXOДИMOCTИ.
   !
   !     ИHФOPMAЦИЯ HA BЫXOДE
   !
   !     Y(*)    -PEШEHИE B TOЧKE T
   !     T       -ПOCЛEДHЯЯ TOЧKA,ДOCTИГHУTAЯ ПPИ ИHTEГPИPOBAHИИ.
   !     IFLAG=2 -ПPИИHTEГPИPOBAHИИ ДOCTИГHУTO TOUT.ЭTO ЗHAЧEHИE
   !              ПAPAMETPA УKAЗЫBAET HA УCПEШHЫЙ BЫXOД И
   !              ЯBЛЯETCЯ HOPMAЛЬHЫM PEЖИMOM ДЛЯ ПPOДOЛЖEHИЯ
   !              ИHTEГPИPOBAHИЯ.
   !          =3 -ИHTEГPИPOBAHИE HE БЫЛO ЗAKOHЧEHO ИЗ-ЗA TOГO,
   !              ЧTO ЗAДAHHOE ЗHAЧEHИE ГPAHИЦЫ ДЛЯ OTHOCИTEЛЬHOЙ
   !              OШИБKИ OKAЗAЛOCЬ CЛИШKOM MAЛO. ДЛЯ ПPOДOЛЖEHИЯ
   !              ИHTEГPИPOBAHИЯ RELERR БЫЛO HAДЛEЖAЩИM OБPAЗOM
   !              УBEЛИЧEHO.
   !          =4 -ИHTEГPИPOBAHИE HE БЫЛO ЗAKOHЧEHO ИЗ-ЗA TOГO,
   !              ЧTO ПOTPEБOBAЛOCЬ БOЛEE 3000 BЫЧИCЛEHИЙ ПPO-
   !              ИЗBOДHOЙ.ЭTO COOTBETCTBYET ПPИБЛИЗИTEЛЬHO
   !              500 ШAГAM.
   !          =5 -ИHTEГPИPOBAHИE HE БЫЛO ЗAKOHЧEHO ИЗ-ЗA TOГO,
   !              ЧTO PEШEHИE OБPATИЛOCЬ B HYЛЬ,BCЛEДCTBИE ЧEГO
   !              TECT TOЛЬKO OTHOCИTEЛЬHOЙ OШИБKИ HE ПPOXOДИT.
   !              ДЛЯ ПPOДOЛЖEHИЯ HEOБXOДИMO HEHYЛEBOE ЗHAЧEHИE
   !              ПAPAMETPA ABSERR. ИCПOЛЬЗOBAHИE HA OДИH ШAГ
   !              PEЖИMA ПOШAГOBOГO ИHTEГPИPOBAHИЯ ЯBЛЯETCЯ
   !              PAЗYMHЫM BЫXOДOM ИЗ ПOЛOЖEHИЯ.
   !          =6 -ИHTEГPИPOBAHИE  HE БЫЛO ЗAKOHЧEHO ИЗ-ЗA TOГO,
   !              ЧTO TPEБYEMAЯ TOЧHOCTЬ HE MOГЛA БЫTЬ ДOCTИГHУTA
   !              ДAЖE ПPИ HAИMEHЬШEЙ ДOПУCTИ MOЙ BEЛИЧИHE ШAГA.
   !              ПOЛЬЗOBATEЛЬ ДOЛЖEH УBEЛИЧИTЬ ГPAHИЦУ ПOГPEШ-
   !              HOCTИ,ПPEЖДE ЧEM MOЖHO БУДET ПOПЫTATЬCЯ
   !              ПPOДOЛЖATЬ ИHTEГPИPOBAHИE.
   !          =7 -ПO BCEЙ BИДИMOCTИ, RKF45 HEЭФФEKTИBHA ПPИ
   !              PEШEHИИ ЭTOЙ ЗAДAЧИ. CЛИШKOM БOЛЬШOE ЧИCЛO
   !              TPEБУEMЫX BЫXOДHЫX TOЧEK ПPEПЯTCTBУET BЫБOPУ
   !              ECTECTBEHHOЙ BEЛИЧИHЫ ШAГA.CЛEДУET ИCПOЛЬЗOBATЬ
   !              PEЖИM ПOШAГOBOГO ИHTEГPИPOBAHИЯ.
   !          =8 -HEПPABИЛЬHOE ЗAДAHИE BXOДHЫX ПAPAMETPOB.ЭTO
   !              ЗHAЧEHИE ПOЯBЛЯETCЯ,ECЛИ ДOПУЩEHA OДHA ИЗ
   !              CЛEДУЮЩИX OШИБOK-
   !                              NEQN.LE.0
   !                  T=TOUT  И  IFLAG.NE.+1  ИЛИ -1
   !                  RELERR  ИЛИ  ABSERR.LT.0
   !                  IFLAG.EQ.0  ИЛИ .LT.-2  ИЛИ .GT.8
   !     WORK(*) -ИHФOPMAЦИЯ, KOTOPAЯ OБЫЧHO HE ПPEДCTABЛЯET ИHTE-
   !              PECA ДЛЯ ПOЛЬЗOBATEЛЯ, HO HEOБXOДИMA ПPИ ПOCЛE-
   !              ДУЮЩИX BЫЗOBAX. WORK(1),...,WORK(NEQN) COДEPЖAT
   !              ПEPBЫE ПPOИЗBOДHЫE BEKTOPA PEШEHИЯ Y B TOЧKE T.
   !              WORK(NEQN+1) XPAHИT BEЛИЧИHУ ШAГA H,C KOTOPOЙ
   !              MOЖHO ПOПЫTATЬCЯ ПPOBECTИ CЛEДУЮЩИЙ ШAГ.
   !     IWORK(*) -ИHФOPMAЦИЯ, KOTOPAЯ OБЫЧHO HE ПPEДCTABЛЯET ИHTE-
   !               PECA ДЛЯ ПOЛЬЗOBATEЛЯ, HO HEOБXOДИMA ПPИ ПOCЛE-
   !               ДУЮЩИX BЫЗOBAX. B IWORK(1) COДEPЖИTCЯ
   !               CЧETЧИK ЧИCЛA BЫЧИCЛEHИЙ ПPOИЗBOДHЫX.
   !
   !     ПOCЛEДУЮЩИE OБPAЩEHИЯ K RKF45
   !
   !          HA BЫXOДE ПOДПPOГPAMMЫ RKF45 ИMEETCЯ BCЯ ИHФOPMAЦИЯ,
   !     HEOБXOДИMAЯ  ДЛЯ ПPOДOЛЖEHИЯ ИHTEГPИPOBAHИЯ.ECЛИ ПPИ
   !     ИHTEГPИPOBAHИИ ДOCTИГHУTO TOUT,TO ПOЛЬЗOBATEЛЮ ДOCTA-
   !     TOЧHO OПPEДEЛИTЬ HOBOE ЗHAЧEHИE  TOUT И CHOBA OБPATИTЬ-
   !     CЯ K RKF45.
   !          B PEЖИME ПOШAГOBOГO ИHTEГPИPOBAHИЯ (IFLAG=-2)
   !     ПOЛЬЗOBATEЛЬ ДOЛЖEH ИMETЬ B BИДУ,ЧTO KAЖДЫЙ ШAГ
   !     BЫПOЛHЯETCЯ B HAПPABЛEHИИ TEKУЩEГO ЗHAЧEHИЯ TOUT
   !     (CИГHAЛИЗИPУEMOM  ИЗMEHEHИEM IFLAG HA 2). ПOЛЬЗOBATEЛЬ
   !     ДOЛЖEH ЗAДATЬ HOBOE ЗHAЧEHИE TOUT И ПEPEOПPEДEЛИTЬ
   !     IFLAG HA -2, ЧTOБЫ ПPOДOЛЖATЬ B PEЖИME ПOШAГOBOГO
   !     ИHTEГPИPOBAHИЯ.
   !          ECЛИ ИHTEГPИPOBAHИE HE БЫЛO ЗAKOHЧEHO,HO
   !     ПOЛЬЗOBATEЛЬ XOЧET ПPOДOЛЖATЬ (CЛУЧAИ IFLAG=3,4), OH
   !     ПOПPOCTУ CHOBA OБPAЩAETCЯ K RKF45.ПPИ IFLAG=3 ПAPA-
   !     METP RELERR БЫЛ  ИЗMEHEH HAДЛEЖAЩИM ДЛЯ ПPOДOЛЖEHИЯ
   !     ИHTEГPИPOBAHИЯ OБPAЗOM.B CЛУЧAE IFLAG=4 CЧETЧИK
   !     ЧИCЛA ЗHAЧEHИЙ ФУHKЦИИ БУДET ПEPEOПPEДEЛEH HA 0, И
   !     БУДУT PAЗPEШEHЫ EЩE 3000 BЫЧИCЛEHИЙ ФУHKЦИИ.
   !          OДHAKO B CЛУЧAE IFLAG=5, ПPEЖДE ЧEM MOЖHO БУДET
   !     ПPOДOЛЖATЬ ИHTEГPИPOBAHИE,ПOЛЬЗOBATEЛЬ ДOЛЖEH CHAЧAЛA
   !     ИЗMEHИTЬ KPИTEPИЙ OШИБKИ, ЗAДAB ПOЛOЖИTEЛЬHOE ЗHAЧEHИE
   !     ДЛЯ ABSERR. ECЛИ OH HE CДEЛAET ЭTOГO, BЫПOЛHEHИE ПPO-
   !     ГPAMMЫ БУДET ПPEKPAЩEHO.
   !        TOЧHO TAK ЖE,B CЛУЧAE IFLAG=6,ПPEЖДE ЧEM ПPOДOЛ-
   !     ЖATЬ ИHTEГPИPOBAHИE,ПOЛЬЗOBATEЛЮ HEOБXOДИMO ПEPEOПPE-
   !     ДEЛИTЬ IFLAG HA 2 (ИЛИ -2, ECЛИ ИCПOЛЬЗУETCЯ PEЖИM
   !     ПOШAГOBOГO ИHTEГPИPOBAHИЯ) И УBEЛИЧИTЬ ЗHAЧEHИE ДЛЯ
   !     ABSERR ЛИБO RELERR,ЛИБO И ДЛЯ TOГO,И ДЛЯ ДPУГOГO.
   !     ECЛИ ЭTO HE БУДET CДEЛAHO,BЫПOЛHEHИE ПPOГPAMMЫ
   !     ПPEKPAЩAETCЯ. ПOЯBЛEHИE IFLAG=6 УKAЗЫBAET HA HEPEГУ-
   !     ЛЯPHOCTЬ (PEШEHИE БЫCTPO MEHЯETCЯ ИЛИ, BOЗMOЖHO,
   !     ИMEETCЯ OCOБEHHOCTЬ),И ЧACTO B ПOДOБHЫX CЛУЧAЯX
   !     HE ИMEET CMЫCЛA ПPOДOЛЖATЬ ИHTEГPИPOBAHИE.
   !          ECЛИ БУДET ПOЛУЧEHO ЗHAЧEHИE IFLAG=7,TO ПOЛЬЗO-
   !     BATEЛЬ ДOЛЖEH ПEPEЙTИ K PEЖИMУ ПOШAГOBOГO ИHTEГPИPO-
   !     BAHИЯ C BEЛИЧИHOЙ ШAГA,OПPEДEЛЯEMOЙ ПPOГPAMMOЙ, ИЛИ
   !     PACCMOTPETЬ BOЗMOЖHOCTTЬ ПEPEXOДA HA ПPOГPAMMЫ METOДOB
   !     AДAMCA.ECЛИ BCE ЖE ПOЛЬЗOBATEЛЬ XOЧET ПPOДOЛЖATЬ
   !     ИHTEГPИPOBAHИE ПO ПOДПPOГPAMME RKF45,OH ДOЛЖEH ДO HOBOГO
   !     OБPAЩEHИЯ K HEЙ ПEPEOПPEДEЛИTЬ IFLAG HA 2.B ПPOTИBHOM
   !     CЛУЧAE BЫПOЛHEHИE ПPOГPAMMЫ БУДET ПPEKPAЩEHO.
   !          ECЛИ ПOЛУЧEHO ЗHAЧEHИE IFLAG=8,TO ИHTEГPИPOBAHИE
   !     HEЛЬЗЯ ПPOДOЛЖATЬ,ПOKA HE БУДУT ИCПPABЛEHЫ OШИБOЧHЫE
   !     BXOДHЫE ПAPAMETPЫ. HУЖHO OTMETИTЬ,ЧTO MACCИBЫ WORK И
   !     IWORK COДEPЖAT ИHФOPMAЦИЮ,HEOБXOДИMУЮ ДЛЯ ДAЛЬHEЙШEГO
   !     ИHTEГPИPOBAHИЯ.ПOЭTOMУ B ЭTИ MACCИBЫ HEЛЬЗЯ BHOCИTЬ
   !     ИЗMEHEHИЙ.
   !
   EXTERNAL F
   INTEGER NEQN, IFLAG, IWORK(5)
   REAL Y(NEQN), T, TOUT, RELERR, ABSERR, WORK(1)
   !
   !     ECЛИ TPAHCЛЯTOP ПPOBEPЯET ИHДEKCЫ, TO ЗAMEHИTЬ
   !     WORK(1) HA WORK(3+6*NEQN)
   !
   INTEGER K1, K2, K3, K4, K5, K6, K1M
   !
   !     BЫЧИCЛИTЬ ИHДEKCЫ ДЛЯ PACЩEПЛEHИЯ PAБOЧEГO MACCИBA
   !
   K1M = NEQN + 1
   K1 = K1M + 1
   K2 = K1 + NEQN
   K3 = K2 + NEQN
   K4 = K3 + NEQN
   K5 = K4 + NEQN
   K6 = K5 + NEQN
   !
   !     ЭTA ПPOMEЖYTOЧHAЯ ПPOГPAMMA ПPOCTO COKPAЩAET ДЛЯ
   !     ПOЛЬЗOBATEЛЯ ДЛИHHЫЙ CПИCOK BЫЗOBA ПYTEM PACЩEПЛEHИЯ
   !     ДBYX PAБOЧИX MACCИBOB. ECЛИ ЭTO HE COBMECTИMO C
   !     TPAHCЛЯTOPOM,TO OH ДOЛЖEH OБPAЩATЬCЯ HEПOCPEДCTBEHHO
   !     K ПOДПPOГPAMME RKFS .
   !
   CALL RKFS(F, NEQN, Y, T, TOUT, RELERR, ABSERR, IFLAG, &
      WORK(1), WORK(K1M), WORK(K1), WORK(K2), &
      WORK(K3), WORK(K4), WORK(K5), WORK(K6), &
      WORK(K6 + 1), IWORK(1), IWORK(2), &
      IWORK(3), IWORK(4), IWORK(5))
   RETURN
END