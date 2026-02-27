
module multiarm_bandit_module
    IMPLICIT NONE

    ! Create a single multi arm bandit (slot machine) object
    type :: multiarm_bandit

        ! Initialized reward and weight of each reward as 1D array
        integer, allocatable :: weight (:)
        integer, allocatable :: reward (:)

        ! Cumulative reward, pull count and expected value
        integer :: total_reward
        integer :: pull_count
        real :: expected_reward

        ! multiarm_bandit's method implementation
        contains
            procedure :: init_bandit
            procedure :: set_reward
            procedure :: pull


    end type multiarm_bandit
    contains
        subroutine init_bandit(this)
            IMPLICIT NONE

            ! Create the result of the multiarm_bandit object initialization
            class (multiarm_bandit), intent(inout) :: this
            real :: rand_num 

            ! Normal init give multiarm_bandit 2 random reward value (0,1) and random weight
            ! Check if memory allocated, if so, deallocated first
            if (allocated(this%weight)) then
                deallocate(this%weight)
            end if

            if (allocated(this%reward)) then
                deallocate(this%reward)
            end if

            ! Allocate memory for 2 slot
            allocate(this%reward(2))
            allocate(this%weight(2))


            ! Set Reward to 0 and 1 respectively
            this%reward(1) = 0
            this%reward(2) = 1

            ! Seed random number from OS for generating pseudo-random number
            CALL RANDOM_SEED() 

            ! Random weight between 1-10, and another as 10 - a
            CALL RANDOM_NUMBER(rand_num)
            this%weight(1) = floor(rand_num * 10) + 1
            this%weight(2) = 10 - this%weight(1)

            ! Initialized expected reward with extremely high value to force a take when exploit
            this%total_reward = 0
            this%pull_count = 0
            this%expected_reward = 1e38

        end subroutine init_bandit

        subroutine set_reward(this, reward_in, weight_in)
            IMPLICIT NONE

            ! Create result class of this function
            class (multiarm_bandit), intent(inout) :: this

            ! Create 2 variable for function input
            integer, intent(in) :: reward_in (:)
            integer, intent(in) :: weight_in (:)

            ! Deallocate old value
            if (allocated(this%weight)) deallocate(this%weight)
            if (allocated(this%reward)) deallocate(this%reward)

            ! Allocate new array
            allocate(this%weight(SIZE(weight_in)))
            allocate(this%reward(SIZE(reward_in)))

            ! Put input to multiarm bandit's attribute

            this%weight = weight_in
            this%reward = reward_in

        end subroutine set_reward

        function pull(this) result(res)
            IMPLICIT NONE

            ! Initialized input of multiarm bandit class
            class (multiarm_bandit), intent(inout) :: this

            ! Declare other variable used in function
            integer :: res, i
            real :: rand_num

            ! Seed random number from OS for generating pseudo-random number
            CALL RANDOM_SEED()

            ! Random the reward with cap at total weight
            CALL RANDOM_NUMBER(rand_num)
            res = FLOOR(rand_num * (SUM(this%weight) + 1.0))

            ! Find which reward is return
            do i = 1, SIZE(this%weight)
                res = res - this%weight(i)
                if (res <= 0) then
                    res = this%reward(i)
                    exit
                end if
            end do
            
        end function pull




end module multiarm_bandit_module