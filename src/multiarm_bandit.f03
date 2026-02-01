
module multiarm_bandit_module
    IMPLICIT NONE

    ! Create a single multiarm bandit (slot machine) object
    type :: multiarm_bandit

        ! Initialized reward and weight of each reward as 1D array
        integer, allocatable :: weight (:);
        integer, allocatable :: reward (:);

        ! contains
        !     procedure :: init
        !     procedure :: pull


    end type multiarm_bandit



end module multiarm_bandit_module