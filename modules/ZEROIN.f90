REAL FUNCTION ZEROIN(AX, BX, F, TOL)
   REAL AX, BX, F, TOL
   !
   !     HУЛЬ ФУHKЦИИ F(X) BЫЧИCЛЯETCЯ B ИHTEPBAЛE AX,BX
   !
   !     BXOДHAЯ ИHФOPMAЦИЯ..
   !
   !     AX     ЛEBЫЙ KOHEЦ ИCXOДHOГO ИHTEPBAЛA
   !     BX     ПPABЫЙ KOHEЦ ИCXOДHOГO ИHTEPBAЛA
   !     F      ПOДПPOГPAMMA-ФУHKЦИЯ, KOTOPAЯ BЫЧИCЛЯET F(X)
   !            ДЛЯ ЛЮБOГO X B ИHTEPBAЛE AX BX
   !     TOL    ЖEЛAEMAЯ ДЛИHA ИHTEPBAЛA HEOПPEДEЛEHHOCTИ
   !            KOHEЧHOГO PEЗУЛЬTATA
   !
   !     BЫXOДHAЯ ИHФOPMAЦИЯ...
   !
   !     ZEROIN AБCЦИCCA, AППPOKCИMИPУЮЩAЯ HУЛЬ ФУHKЦИИ F B
   !            ИHTEPBAЛE AX, BX
   !
   !        БEЗ ПPOBEPKИ ПPEДПOЛAГAETCЯ, ЧTO F(AX) И F(BX) ИMEЮT
   !     ПPOTИBOПOЛOЖHЫE ЗHAKИ.
   !        ZEROIN BЫЧИCЛЯET HУЛЬ X B ЗAДAHHOM ИHTEPBAЛE AX, BX
   !     B ПPEДEЛAX ДOПУCKA HA OШИБKУ  4*MACHEPS*ABS(X) + TOL,
   !     ГДE MACHEPS-OTHOCИTEЛЬHAЯ MAШИHHAЯ TOЧHOCTЬ.
   !        ЭTA ПOДПPOГPAMMA-ФУHKЦИЯ ПPEДCTABЛЯET COБOЙ CЛEГKA
   !     MOДИФИЦИPOBAHHУЮ TPAHCЛЯЦИЮ AЛГOЛ 60-ПPOЦEДУPЫ ZERO,
   !     ПPИBEДEHHOЙ B KHИГE RICHARD BRENT, ALGORITHMS FOR
   !     MINIMIZATION WITHOUT DERIVATIVES,PRENTICE HALL,INC.(1973).
   !
   REAL A, B, C, D, E, EPS, FA, FB, FC, TOL1, XM, P, Q, R, S
   !
   !     BЫЧИCЛИTЬ EPS,OTHOCИTEЛЬHУЮ MAШИHHУЮ TOЧHOCTЬ
   !
   EPS = 1.0
   10 EPS = EPS / 2.0
   TOL1 = 1.0 + EPS
   IF(TOL1>1.0) GO TO 10
   !
   !     ПPИCBOEHИE HAЧAЛЬHЫX ЗHAЧEHИЙ
   !
   A = AX
   B = BX
   FA = F(A)
   FB = F(B)
   !
   !     HAЧATЬ ШAГ
   !
   20 C = A
   FC = FA
   D = B - A
   E = D
   30 IF(ABS(FC)>=ABS(FB)) GO TO 40
   A = B
   B = C
   C = A
   FA = FB
   FB = FC
   FC = FA
   !
   !     ПPOBEPKA CXOДИMOCTИ
   !
   40 TOL1 = 2.0 * EPS * ABS(B) + 0.5 * TOL
   XM = 0.5 * (C - B)
   IF(ABS(XM)<=TOL1) GO TO 90
   !
   !     HEOБXOДИMA ЛИ БИCEKЦИЯ
   !
   IF(FB==0.0) GO TO 90
   IF(ABS(E)<TOL1) GO TO 70
   IF(ABS(FA)<=ABS(FB)) GO TO 70
   !
   !     BOЗMOЖHA ЛИ KBAДPATИЧHAЯ ИHTEPПOЛЯЦИЯ
   !
   IF(A/=C)GO TO 50
   !
   !     ЛИHEЙHAЯ ИHTEPПOЛЯЦИЯ
   !
   S = FB / FA
   P = 2.0 * XM * S
   Q = 1.0 - S
   GO TO 60
   !
   !     OБPATHAЯ KBAДPATИЧHAЯ ИHTEPПOЛЯЦИЯ
   !
   50 Q = FA / FC
   R = FB / FC
   S = FB / FA
   P = S * (2.0 * XM * Q * (Q - R) - (B - A) * (R - 1.0))
   Q = (Q - 1.0) * (R - 1.0) * (S - 1.0)
   !
   !     BЫБPATЬ ЗHAKИ
   !
   60 IF(P>0.0) Q = -Q
   P = ABS(P)
   !
   !     ПPИEMЛEMA ЛИ ИHTEPПOЛЯЦИЯ
   !
   IF((2.0 * P)>=(3.0 * XM * Q - ABS(TOL1 * Q))) GO TO 70
   IF(P>=ABS(0.5 * E * Q)) GO TO 70
   E = D
   D = P / Q
   GO TO 80
   !
   !     БИCEKЦИЯ
   !
   70 D = XM
   E = D
   !
   !     ЗABEPШИTЬ ШAГ
   !
   80 A = B
   FA = FB
   IF(ABS(D)>TOL1) B = B + D
   IF(ABS(D)<=TOL1) B = B + SIGN(TOL1, XM)
   FB = F(B)
   IF((FB * (FC / ABS(FC)))>0.0) GO TO 20
   GO TO 30
   !
   !     KOHЧEHO
   !
   90 ZEROIN = B
   RETURN
END