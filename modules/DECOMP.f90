SUBROUTINE DECOMP(NDIM, N, A, COND, IPVT, WORK)
   !
   INTEGER NDIM, N
   REAL A(NDIM, N), COND, WORK(N)
   INTEGER IPVT(N)
   !
   !     ПPOГPAMMA BЫЧИCЛЯET PAЗЛOЖEHИE BEЩECTBEHHOЙ MATPИЦЫ
   !     ПOCPEДCTBOM ГAУCCOBA ИCKЛЮЧEHИЯ И OЦEHИBAET
   !     OБУCЛOBЛEHHOCTЬ  MATPИЦЫ.
   !
   !     OHA ИCПOЛЬЗУETCЯ ДЛЯ BЫЧИCЛEHИЯ PEШEHИЙ
   !     ЛИHEЙHЫX CИCTEM.
   !
   !     BXOДHAЯ ИHФOPMAЦИЯ.
   !
   !     NDIM -ЗAЯBЛEHHAЯ CTPOЧHAЯ PAЗMEPHOCTЬ MACCИBA,
   !           COДEPЖAЩEГO A.
   !
   !     N    -ПOPЯДOK MATPИЦЫ.
   !
   !     A    -MATPИЦA,KOTOPУЮ HУЖHO PAЗЛOЖИTЬ.
   !
   !     BЫXOДHAЯ ИHФOPMAЦИЯ.
   !
   !     A     COДEPЖИT BEPXHЮЮ TPEУГOЛЬHУЮ MATPИЦУ U
   !           И УЧИTЫBAЮЩУЮ ПEPECTAHOBKИ BEPCИЮ
   !           HИЖHEЙ TPEУГOЛЬHOЙ MATPИЦЫ I-L,TAKИE,
   !           ЧTO (MATPИЦA  ПEPECTAHOBOK) *A=L*U
   !
   !     COND -OЦEHKA OБУCЛOBЛEHHOCTИ A.
   !           ДЛЯ ЛИHEЙHOЙ CИCTEMЫ A*X=B ИЗMEHEHИЯ B A И B
   !           MOГУT BЫЗBATЬ  ИЗMEHEHИЯ B X,БOЛЬШИE B COND PAЗ.
   !           ECЛИ COND+1.0.EQ.COND, TO  A B ПPEДEЛAX MAШИHHOЙ
   !           TOЧHOCTИ ЯBЛЯETCЯ BЫPOЖДEHHOЙ MATPИЦEЙ. COND
   !           ПOЛAГAETCЯ PABHЫM 1.0E+32,ECЛИ OБHAPУЖEHA TOЧHAЯ
   !           BЫPOЖДEHHOCTЬ.
   !
   !     IPVT -BEKTOP BEДУЩИX ЭЛEMEHTOB.
   !           IPVT(K)=ИHДEKC K-Й BEДУЩEЙ CTPOKИ
   !           IPVT(N)=(-1)**(ЧИCЛO ПEPECTAHOBOK)
   !
   !     PAБOЧEE ПOЛE. BEKTOP WORK ДOЛЖEH БЫTЬ OПИCAH И
   !             BKЛЮЧEH B BЫЗOB. EГO BXOДHOE COДEPЖAHИE OБЫЧHO
   !             HE ДAET BAЖHOЙ ИHФOPMAЦИИ.
   !
   !     OПPEДEЛИTEЛЬ MATPИЦЫ A MOЖET БЫTЬ ПOЛУЧEH HA BЫXOДE
   !     ПO ФOPMУЛE:
   !          DET(A)=IPVT(N)*A(1,1)*A(2,2)*...*A(N,N).
   !
   REAL EK, T, ANORM, YNORM, ZNORM
   INTEGER NM1, I, J, K, KP1, KB, KM1, M
   !
   IPVT(N) = 1
   IF(N==1)GO TO 80
   NM1 = N - 1
   !
   !     BЫЧИCЛИTЬ 1-HOPMУ MATPИЦЫ A
   !
   ANORM = 0.0
   DO J = 1, N
      T = 0.0
      DO I = 1, N
         T = T + ABS(A(I, J))
      end do
      IF(T>ANORM) ANORM = T
   end do
   !
   !     ГAУCCOBO ИCKЛЮЧEHИE C ЧACTИЧHЫM BЫБOPOM
   !     BEДУЩEГO ЭЛEMEHTA
   !
   DO K = 1, NM1
      KP1 = K + 1
      !
      !       HAЙTИ BEДУЩИЙ ЭЛEMEHT
      !
      M = K
      DO I = KP1, N
         IF(ABS(A(I, K))>ABS(A(M, K))) M = I
      end do
      IPVT(K) = M
      IF(M/=K)IPVT(N) = -IPVT(N)
      T = A(M, K)
      A(M, K) = A(K, K)
      A(K, K) = T
      !
      !       ПPOПУCTИTЬ ЭTOT ШAГ,ECЛИ BEДУЩИЙ ЭЛEMEHT PABEH HУЛЮ
      !
      IF(T==0.0)GO TO 35
      !
      !       BЫЧИCЛИTЬ MHOЖИTEЛИ
      !
      DO I = KP1, N
         A(I, K) = -A(I, K) / T
      end do
      !
      !       ПEPECTABЛЯTЬ И ИCKЛЮЧATЬ ПO CTOЛБЦAM
      !
      DO J = KP1, N
         T = A(M, J)
         A(M, J) = A(K, J)
         A(K, J) = T
         IF(T==0.0)GO TO 30
         DO I = KP1, N
            A(I, J) = A(I, J) + A(I, K) * T
         end do
      30   CONTINUE
      end do
   35 CONTINUE
   end do
   !
   !     COND=(1-HOPMA MATPИЦЫ A)*(OЦEHKA ДЛЯ 1-HOPMЫ MATPИЦЫ,
   !     OБPATHOЙ K A)
   !     OЦEHKA ПOЛУЧAETCЯ ПOCPEДCTBOM OДHOГO ШAГA METOДA
   !     OБPATHЫX ИTEPAЦИЙ ДЛЯ HAИMEHЬШEГO CИHГУЛЯPHOГO
   !     BEKTOPA. ЭTO TPEБУET PEШEHИЯ ДBУX CИCTEM УPABHEHИЙ,
   !     (TPAHCПOHИPOBAHHAЯ ДЛЯ A) *Y=E И A*Z=Y, ГДE E-BEKTOP
   !     ИЗ +1 И -1, BЫБPAHHЫЙ TAK, ЧTOБЫ MAKCИMИЗИPOBATЬ
   !     BEЛИЧИHУ Y.
   !     ESTIMATE=(1-HOPMA Z)/(1-HOPMA Y)
   !
   !     PEШИTЬ CИCTEMУ (TPAHCПOHИPOBAHHAЯ ДЛЯ A)*Y=E
   !
   DO K = 1, N
      T = 0.0
      IF(K==1)GO TO 45
      KM1 = K - 1
      DO I = 1, KM1
         T = T + A(I, K) * WORK(I)
      end do
      45   EK = 1.0
      IF(T<0.0)EK = -1.0
      IF(A(K, K)==0.0)GO TO 90
      WORK(K) = -(EK + T) / A(K, K)
   end do
   DO KB = 1, NM1
      K = N - KB
      T = WORK(K)
      KP1 = K + 1
      DO I = KP1, N
         T = T + A(I, K) * WORK(I)
      end do
      WORK(K) = T
      M = IPVT(K)
      IF(M==K)GO TO 60
      T = WORK(M)
      WORK(M) = WORK(K)
      WORK(K) = T
   60 CONTINUE
   end do
   !
   YNORM = 0.0
   DO I = 1, N
      YNORM = YNORM + ABS(WORK(I))
   end do
   !
   !     PEШИTЬ CИCTEMУ A*Z=Y
   !
   CALL SOLVE(NDIM, N, A, WORK, IPVT)
   !
   ZNORM = 0.0
   DO I = 1, N
      ZNORM = ZNORM + ABS(WORK(I))
   end do
   !
   !     OЦEHИTЬ OБУCЛOBЛEHHOCTЬ
   !
   COND = ANORM * ZNORM / YNORM
   IF(COND<1.0)COND = 1.0
   RETURN
   !
   !     CЛУЧAЙ MATPИЦЫ 1*1
   !
   80 COND = 1.0
   IF(A(1, 1)/=0.0)RETURN
   !
   !     TOЧHAЯ BЫPOЖДEHHOCTЬ
   !
   90 CONTINUE
   COND = 1.0E+32
   RETURN
END
