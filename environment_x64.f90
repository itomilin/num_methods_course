module Environment
   use ISO_Fortran_Env

   implicit none
    
   integer, parameter      :: I_ = INT64                             ! Integer kind.
   integer, parameter      :: R_ = REAL64                            ! Float kind.
   integer, parameter      :: C_ = R_                                ! Complex kind.
   integer, parameter      :: CH_= Selected_Char_Kind("ISO_10646")   ! Character kind.
   character(*), parameter :: E_ = "UTF-8"                           ! Files encoding.

   interface operator (//)
      module procedure Int_plus_string
      module procedure String_plus_int
   end interface

contains

   pure function Int_plus_string(int, str) result(res)
      integer(I_), intent(in)                                     :: int
      character(*), intent(in)                                    :: str
      character(len(str)+Max(Floor(Log10(Real(int, I_*2)))+1, 1)) :: res

      write (res, '(i0, a)') int, str
   end function Int_plus_string

   pure function String_plus_int(str, int) result(res)
      character(*), intent(in)                                    :: str
      integer(I_), intent(in)                                     :: int
      character(len(str)+Max(Floor(Log10(Real(int, I_*2)))+1, 1)) :: res

      write (res, '(a, i0)') str, int
   end function String_plus_int

   ! Обработка статуса ввода/вывода.
   subroutine Handle_IO_status(IO, where)
      integer(I_), intent(in)    :: IO
      character(*), intent(in)   :: where

      open (ERROR_UNIT, encoding=E_)
      select case(IO)
         case(0, IOSTAT_END, IOSTAT_EOR)
         case(1:)
            write (ERROR_UNIT, '(a, i0)') "Error " // where // ": ", IO
         case default
            write (ERROR_UNIT, '(a, i0)') "Undetermined behaviour has been reached while " // where // ": ", IO
      end select
      ! close (Out) ! Если не OUTPUT_UNIT.
   end subroutine Handle_IO_status

end module Environment
