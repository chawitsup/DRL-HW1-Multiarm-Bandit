
module multiarm_bandit_module
    IMPLICIT NONE

    ! Create a single multi arm bandit (slot machine) object
    type :: multiarm_bandit

        ! Initialized reward and weight of each reward as 1D array
        integer :: reward_size
        integer, allocatable :: weight (:)
        integer, allocatable :: reward (:)

        contains
            procedure :: init
            ! procedure :: set_reward
            ! procedure :: pull


    end type multiarm_bandit
    contains
        subroutine init(this)
            
            IMPLICIT NONE

            ! Create the result of the multiarm_bandit object
            class (multiarm_bandit), intent(inout) :: this
            real :: rand_num 

            ! Normal init give multiarm_bandit 2 random reward value
            ! Check if allocated, if so, deallocated first
            if (allocated(this%weight)) then
                deallocate(this%weight)
            end if

            if (allocated(this%reward)) then
                deallocate(this%reward)
            end if

            ! Allocate memory for 2 slot
            allocate(this%reward(2))
            allocate(this%weight(2))

            ! Random value of reward
            CALL RANDOM_SEED() 

            CALL RANDOM_NUMBER(rand_num)
            this%reward(1) = floor(rand_num * 11)

            CALL RANDOM_NUMBER(rand_num)
            this%reward(2) = floor(rand_num * 11)

            CALL RANDOM_NUMBER(rand_num)
            this%weight(1) = floor(rand_num * 10) + 1

            CALL RANDOM_NUMBER(rand_num)
            this%weight(2) = floor(rand_num * 10) + 1


        end subroutine init



end module multiarm_bandit_module