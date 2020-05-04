real function function_e( t )
    real :: t

    function_e = 1.0 / sqrt( ( t ** 2 + 1 ) * ( 3 * t ** 2 + 4 ) )

    ! nik
    !function_e = ((1.0 - exp(-0.8*t))/(t*(1 + 1.6 * t)))
    return
end


program coursework

    external function_e

    integer :: NOFUN = 0, &
               i     = 0, &
               j     = 0

    ! --------------------------- FUNCTION_E -------------------------

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

    ! ----------------------------------------------------------------

    print *, "Hello World!"

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

    print *, quanc_output

    result_e = const_value_e * quanc_output
    ! nik
    !result_e = ( quanc_output - 0.40874702 ) ** 4
    print *, result_e

    ! ----------------------------------------------------------------

end program coursework
