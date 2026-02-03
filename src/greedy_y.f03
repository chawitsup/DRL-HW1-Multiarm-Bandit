module greedy_y_agent_module
    use multiarm_bandit_module

    IMPLICIT NONE

    ! Declare agent object
    type :: greedy_y_agent

        ! Global agent's parameter and hyperparameter
        type (multiarm_bandit), allocatable :: bandit_instance (:)
        real :: epsilon_value

        ! Array to store expected value for each multi arm bandit instance
        integer, allocatable :: total_reward (:)
        integer, allocatable :: pull_count (:)

        contains
            procedure :: init_y_agent
            procedure :: reset_y_agent

    end type greedy_y_agent

    contains

        subroutine init_y_agent(this, bandit_in, epsilon_in)
            IMPLICIT NONE

            ! Declare output object of greedy_y_agent
            class (greedy_y_agent), intent(inout) :: this

            ! Declare input variable
            type (multiarm_bandit), intent(in) :: bandit_in (:)
            real, intent(in) :: epsilon_in

            ! Internal variable
            integer :: i

            ! Check if epsilon is between 0 and 1
            if (epsilon_in < 0 .or. epsilon_in > 1) then
                error stop "Epsilon in must be between 0 and 1."
            end if

            ! Check and allocate memory for bandit instance
            if (allocated(this%bandit_instance)) deallocate(this%bandit_instance)
            allocate(this%bandit_instance(SIZE(bandit_in)))


            ! Set agent's attribute
            this%bandit_instance = bandit_in
            this%epsilon_value = epsilon_in

            ! Deallocate old array
            if (allocated(this%total_reward)) deallocate(this%total_reward)
            if (allocated(this%pull_count)) deallocate(this%pull_count)

            ! Allocate total reward and pull count to be coresponding to amount of multi arm bandit
            allocate(this%total_reward(SIZE(this%bandit_instance)))
            allocate(this%pull_count(SIZE(this%bandit_instance)))

            ! Initialized all value to 0
            do i = 1, SIZE(this%bandit_instance)
                this%total_reward(i) = 0
                this%pull_count(i) = 0
            end do

        end subroutine init_y_agent


        subroutine reset_y_agent(this)
            IMPLICIT NONE

            ! Declare output object of greedy_y_agent
            class (greedy_y_agent), intent(inout) :: this

            integer :: i

            ! Set all value to 0
            do i = 1, SIZE(this%bandit_instance)
                this%total_reward(i) = 0
                this%pull_count(i) = 0
            end do

        end subroutine reset_y_agent

    ! function iter_y_agent(this) result(n)
    !     IMPLICIT NONE



    ! end function iter_y_agent







    



end module greedy_y_agent_module