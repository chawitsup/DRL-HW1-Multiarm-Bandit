module greedy_y_agent_module
    use multiarm_bandit_module

    IMPLICIT NONE

    ! Declare agent object
    type :: greedy_y_agent

        ! Global agent's parameter and hyperparameter
        type (multiarm_bandit), allocatable :: bandit_instance (:)
        real :: epsilon_value

        ! Logger parameter
        integer :: log_fd
        

        contains
            procedure :: init_y_agent
            procedure :: reset_y_agent
            procedure :: iter_y_agent

    end type greedy_y_agent

    contains

        subroutine init_y_agent(this, bandit_in, epsilon_in)
            IMPLICIT NONE

            ! Declare output object of greedy_y_agent
            class (greedy_y_agent), intent(inout) :: this

            ! Declare input variable
            type (multiarm_bandit), intent(in) :: bandit_in (:)
            real, intent(in) :: epsilon_in

            ! Logger status
            integer :: return_stat

            ! Internal variable
            integer :: i

            

            ! Open file
            ! open(unit=this%log_fd, status=new, action="READWRITE", iostat=return_stat)
            ! if (return_stat /= 0) then
            !     print *, "Failed to open log file."
            !     stop
            ! end if

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

        end subroutine init_y_agent


        subroutine reset_y_agent(this)
            IMPLICIT NONE

            ! Declare output object of greedy_y_agent
            class (greedy_y_agent), intent(inout) :: this

            integer :: i

            ! Set all value to 0
            do i = 1, SIZE(this%bandit_instance)
                this%bandit_instance(i)%total_reward = 0
                this%bandit_instance(i)%pull_count = 0
                this%bandit_instance(i)%expected_reward = 0
            end do

        end subroutine reset_y_agent

        function iter_y_agent(this) result(n)
            IMPLICIT NONE

            ! Declare agent object as input, n as agent's output
            class (greedy_y_agent), intent(inout) :: this
            integer :: n

            ! Internal variable
            integer :: i
            real :: rand_num
            integer :: choosen_instance

            ! Random for epsilon value
            call RANDOM_SEED()
            call RANDOM_NUMBER(rand_num)

            ! Choose between explore and exploit
            if (rand_num < this%epsilon_value) then

                ! Explore: Random which arm to explore
                call RANDOM_NUMBER(rand_num)
                choosen_instance = (rand_num * SIZE(this%bandit_instance)) + 1
            
            ! Choose which to exploit
            else
                ! Find expected value of each 
                choosen_instance = 1

                ! Linear search for max expected value
                do i = 2, SIZE(this%bandit_instance)
                    if (this%bandit_instance(i)%expected_reward > this%bandit_instance(choosen_instance)%expected_reward) then
                        choosen_instance = i

                        ! When two instance has tie expected_reward, use the chosen_instance

                    ! else if (this%bandit_instance(i)%expected_reward == this%bandit_instance(choosen_instance)%expected_reward)
                        ! TODO: Implement other tie breaker algorithm later

                    end if

                end do

            end if

            ! Pull the chosen arm
            n = pull(this%bandit_instance(choosen_instance))

            ! Update expected value
            this%bandit_instance(choosen_instance)%total_reward = this%bandit_instance(choosen_instance)%total_reward + n
            this%bandit_instance(choosen_instance)%pull_count = this%bandit_instance(choosen_instance)%pull_count + 1
            this%bandit_instance(choosen_instance)%expected_reward = this%bandit_instance(choosen_instance)%total_reward / this%bandit_instance(choosen_instance)%pull_count

            ! Log value


        end function iter_y_agent


        ! Add logging function
        ! subroutine start_log_y_agent(this)
            
        ! end subroutine
        







    



end module greedy_y_agent_module