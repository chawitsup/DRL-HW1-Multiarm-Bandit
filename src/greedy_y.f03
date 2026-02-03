module greedy_y_agent_module
    IMPLICIT NONE

    ! Declare agent object
    type :: greedy_y_agent

        ! Global agent's parameter and hyperparameter
        integer :: bandit_count
        real :: epsilon_value

        ! Array to store expected value for each multi arm bandit instance
        integer, allocatable :: total_reward (:)
        integer, allocatable :: pull_count (:)

        contains
            procedure :: init_y_agent
            procedure :: reset_y_agent

    end type greedy_y_agent

    contains

        subroutine init_y_agent(this, bandit_size_in, epsilon_in)
            IMPLICIT NONE

            ! Declare output object of greedy_y_agent
            class (greedy_y_agent), intent(inout) :: this

            integer, intent(in) :: bandit_size_in
            real, intent(in) :: epsilon_in

            integer :: i

            ! Compile time check: check if epsilon is between 0 and 1
            if (epsilon_in < 0 .or. epsilon_in > 1) then
                error stop "Epsilon in must be between 0 and 1."
            end if

            ! Set agent's attribute
            this%bandit_count = bandit_size_in
            this%epsilon_value = epsilon_in

            ! Deallocate old array
            if (allocated(this%total_reward)) deallocate(this%total_reward)
            if (allocated(this%pull_count)) deallocate(this%pull_count)

            ! Allocate total reward and pull count to be coresponding to amount of multi arm bandit
            allocate(this%total_reward(this%bandit_count))
            allocate(this%pull_count(this%bandit_count))

            ! Initialized all value to 0
            do i = 1, this%bandit_count
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
            do i = 1, this%bandit_count
                this%total_reward(i) = 0
                this%pull_count(i) = 0
            end do

        end subroutine reset_y_agent

    ! function iter_y_agent(this) result(n)
    !     IMPLICIT NONE



    ! end function iter_y_agent







    



end module greedy_y_agent_module