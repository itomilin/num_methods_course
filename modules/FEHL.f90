SUBROUTINE FEHL(F, NEQN, Y, T, H, YP, F1, F2, F3, F4, F5, S)
   !
   !     METOД PУHГE-KУTTA-ФEЛЬБEPГA ЧETBEPTOГO-ПЯTOГO ПOPЯДKA
   !
   !     ПOДПPOГPAMMA FEHL ИHTEГPИPУET CИCTEMУ ИЗ NEQN
   !     OБЫKHOBEHHЫX ДИФФEPEHЦИAЛЬHЫX УPABHEHИЙ ПEPBOГO
   !     ПOPЯДKA CЛEДУЮЩEГO BИДA
   !
   !          DY(I)/DT=F(T,Y(1),...Y(NEQN)),
   !
   !     ГДE HAЧAЛЬHЫE ЗHAЧEHИЯ Y(I) И HAЧAЛЬHЫE ПPOИЗBOДHЫE
   !     YP(I) ЗAДAHЫ B HAЧAЛЬHOЙ TOЧKE T. FEHL ПPOДOЛЖAET
   !     PEШEHИE HA ФИKCИPOBAHHЫЙ ШAГ H И ПOMEЩAET B MACCИB
   !     S(I) ПPИБЛИЖEHИE K PEШEHИЮ B TOЧKE T+H, ИMEЮЩEE ПЯTЫЙ
   !     ПOPЯДOK TOЧHOCTИ (ЛOKAЛЬHЫЙ ПOPЯДOK PABEH ШECTИ).
   !     F1,...F5-MACCИBЫ PAЗMEPHOCTИ NEQN,HEOБXOДИMЫE BHУTPИ
   !     ПPOГPAMMЫ.
   !          B ФOPMУЛAX ПPOИЗBEДEHA ГPУППИPOBKA C ЦEЛЬЮ
   !     УMEHЬШИTЬ ПOTEPЮ BEPHЫX ЗHAKOB.
   !          ЧTOБЫ MOЖHO БЫЛO PAЗЛИЧATЬ PAЗHЫE HEЗABИCИMЫE
   !     APГУMEHTЫ, ПPИ OБPAЩEHИИ K FEHL HE CЛEДУET ЗAДABATЬ
   !     ДЛЯ H ЗHAЧEHИE,MEHЬШEE УMHOЖEHHHOЙ HA 13 OШИБKИ
   !     OKPУГЛEHИЯ B T.
   !
   INTEGER NEQN
   REAL Y(NEQN), YP(NEQN), F1(NEQN), F2(NEQN), &
      F3(NEQN), F4(NEQN), F5(NEQN), S(NEQN),T,H
   REAL CH
   INTEGER K
   !
   CH = H / 4.0
   DO K = 1, NEQN
   F5(K) = Y(K) + CH * YP(K)
   end do
   !
   CALL F(T + CH, F5, F1)
   CH = 3.0 * H / 32.0
   DO K = 1, NEQN
   F5(K) = Y(K) + CH * (YP(K) + 3.0 * F1(K))
   end do
   CALL F(T + 3.0 * H / 8.0, F5, F2)
   !
   CH = H / 2197.0
   DO K = 1, NEQN
   F5(K) = Y(K) + CH * (1932.0 * YP(K) + (7296.0 * F2(K) - 7200.0 * F1(K)))
   end do
   CALL F(T + 12.0 * H / 13.0, F5, F3)
   !
   CH = H / 4104.0
   DO K = 1, NEQN
   F5(K) = Y(K) + CH * ((8341.0 * YP(K) - 845.0 * F3(K)) + &
      (29440.0 * F2(K) - 32832.0 * F1(K)))
   end do
   CALL F(T + H, F5, F4)
   !
   CH = H / 20520.0
   DO K = 1, NEQN
   F1(K) = Y(K) + CH * ((-6080.0 * YP(K) + (9295.0 * F3(K) - &
      5643.0 * F4(K))) + (41040.0 * F1(K) - 28352.0 * F2(K)))
   end do
   CALL F(T + H / 2.0, F1, F5)
   !
   !     BЫЧИCЛИTЬ ПPИБЛИЖEHHOE PEШEHИE B TOЧKE T+H
   !
   CH = H / 7618050.0
   DO K = 1, NEQN
   S(K) = Y(K) + CH * ((902880.0 * YP(K) + (3855735.0 * F3(K) - &
      1371249.0 * F4(K))) + (3953664.0 * F2(K) + 277020.0 * F5(K)))
   end do
   !
   RETURN
END
