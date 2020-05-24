real function function_e( t )

    real :: t

    function_e = 1.0 / sqrt( ( t ** 2 + 1 ) * ( 3 * t ** 2 + 4 ) )
    return

end


real function function_a( x )

    real :: x

    function_a = x ** 2 - cos( x )
    return

end


subroutine duffing( t,  &
                    w,  &
                    f )

    real :: t, w( 2 ), f( 2 ), e

    !e = 0.200000
    !e = 0.22
    e = 0.280000

    ! Два уравнения первого порядка
    f( 1 ) = w( 2 ) - ( e * w( 2 ) ** 3 ) ! Производная первой компоненты U
    f( 2 ) = w( 1 )                       ! Производная второй компоненты U'

    return

end


program coursework

    external function_e, &
             function_a, &
             duffing

    integer :: NOFUN = 0

    real :: b = 0.0

    ! QUANC8 vars
    real :: result_e     = 0.0, &
            lower_bound  = 0.0, &
            upper_bound  = 0.0, &
            relerr       = 0.0, &
            abserr       = 0.0, &
            flag         = 0.0, &
            errest       = 0.0, &
            quanc_output = 0.0

    real, parameter :: const_value_e = 0.497286

    ! ZEROIN vars
    real :: result_a   = 0.0, &
            ax         = 0.0, &
            bx         = 0.0, &
            tol        = 0.0, &
            zeroin

    real, parameter :: const_value_a = 1.213399

    ! RKF45 vars
    real :: rwork( 15 ), &
            w( 2 ),      &
            t,           &
            tout,        &
            tfinal,      &
            tprint

    integer :: iwork( 5 ), &
               iflag,      &
               neqn

    ! ----------------------------------------------------------------


    ! -------------------------- CALC_QUANC8 -------------------------

    lower_bound = 1.e-06
    upper_bound = 1.0
    relerr      = 1.e-06
    abserr      = 0.0

    call quanc8( function_e,   &
                 lower_bound,  &
                 upper_bound,  &
                 abserr,       &
                 relerr,       &
                 quanc_output, &
                 errest,       &
                 NOFUN,        &
                 flag )

    result_e = const_value_e * quanc_output
    write ( *, '( A, F8.6 )' ) "E = ", result_e

    ! ----------------------------------------------------------------

    ! -------------------------- CALC_ZEROIN -------------------------

    ax  = -0.1
    bx  = 0.90000
    tol = 1.e-06

    result_a = zeroin( ax, bx, function_a, tol )
    result_a = result_a * const_value_a
    write ( *, '( A, F9.6 )' ) "A = ", result_a

    ! ----------------------------------------------------------------

    ! -------------------------- CALC_RKF45 --------------------------

    neqn   = 2
    w( 1 ) = result_a
    w( 2 ) = b
    t      = 0.000001
    tfinal = 16
    iflag  = 1
    tout   = t
    tprint = 0.4

    10 call RKF45( duffing, &
                   neqn,    &
                   w,       &
                   t,       &
                   tout,    &
                   relerr,  &
                   abserr,  &
                   iflag,   &
                   rwork,   &
                   iwork )

    !write ( *, 11 ) t, w( 1 ), w( 2 )
    write ( *, 11 ) t, w( 2 )
    go to ( 80, 20, 30, 40, 50, 60, 70, 80 ), iflag

    20 tout = tprint + t

    if ( t.lt.tfinal ) go to 10
        stop

    30 write ( *, 31 ) relerr, abserr
        go to 10
    40 write ( *, 41 )
        go to 10
    50 abserr = 0.1e-06
    write ( *, 31 ) relerr, abserr
        go to 10
    60 relerr = relerr * 10.0
    write ( *, 31 ) relerr, abserr

    iflag = 2
    go to 10

    70 print 71
    iflag = 2
    go to 10
    80 write ( *, 81 )

    ! ----------------------------------------------------------------
    !11 format( '(', f10.2, 2x, '; ', f10.6, 2x, '; ', E14.6, ' )' )
    11 format( '(', f10.2, 2x, '; ', E14.6, ' )' )
    !11 format( f5.2, 2x, f10.6, 2x, E14.6 )
    31 format( ' ГPAHИЦЫ ПOГPEШHOCTEЙ ИЗMEHEHЫ  '/' RELERR=', E10.3, 2X, &
               'ABSERR=', E10.3 )
    41 format( ' MHOГO ШAГOB ' )
    71 format( ' MHOГO BЫXOДOB ' )
    81 format( ' HEПPABИЛЬHЫЙ BЫЗOB ' )

end program coursework
