SUBROUTINE SOLVE(NDIM, N, A, B, IPVT)
   !
   INTEGER NDIM, N, IPVT(N)
   REAL A(NDIM, N), B(N)
   !
   !     PEШEHИE ЛИHEЙHOЙ CИCTEMЫ A*X=B
   !     ПOДПPOГPAMMY HE CЛEДYET ИCПOЛЬЗOBATЬ,
   !     ECЛИ DECOMP OБHAPYЖИЛ BЫPOЖДEHHOCTЬ
   !
   !     BXOДHAЯ ИHФOPMAЦИЯ.
   !
   !     NDIM -ЗAЯBЛEHHAЯ CTPOЧHAЯ PAЗMEPHOCTЬ
   !           MACCИBA,COДEPЖAЩEГO A.
   !     N    -ПOPЯДOK MATPИЦЫ.
   !     A    -ФAKTOPИЗOBAHHAЯ MATPИЦA,ПOЛYЧEHHAЯ ИЗ DECOMP
   !     B    -BEKTOP ПPABЫX ЧACTEЙ.
   !     IPVT -BEKTOP BEДYЩИX ЭЛEMEHTOB,ПOЛYЧEHHЫЙ ИЗ DECOMP
   !
   !     BЫXOДHAЯ ИHФOPMAЦИЯ.
   !
   !     B    =BEKTOP PEШEHИЯ X.
   !
   INTEGER KB, KM1, NM1, KP1, I, K, M
   REAL T
   !
   !     ПPЯMOЙ XOД
   !
   IF(N==1) GO TO 50
   NM1 = N - 1
   DO K = 1, NM1
      KP1 = K + 1
      M = IPVT(K)
      T = B(M)
      B(M) = B(K)
      B(K) = T
      DO I = KP1, N
         B(I) = B(I) + A(I, K) * T
      end do
   end do
   !
   !     OБPATHAЯ ПOДCTAHOBKA
   !
   DO KB = 1, NM1
      KM1 = N - KB
      K = KM1 + 1
      B(K) = B(K) / A(K, K)
      T = -B(K)
      DO I = 1, KM1
         B(I) = B(I) + A(I, K) * T
      end do
   end do
   50 B(1) = B(1) / A(1, 1)
   RETURN
END
