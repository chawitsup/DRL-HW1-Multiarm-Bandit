module ucb_agent_module
    use multiarm_bandit_module

    IMPLICIT NONE

    ! Declare agent object
    type :: ucb_agent

        ! Array of multi arm bandit object
        type (multiarm_bandit), allocatable :: bandit_instance (:)

        ! Hyperparameter
        real :: c

        ! Logger file descriptor
        integer :: total_pull = 0
        integer :: log_fd



        ! Agent method
        contains
            
    end type ucb_agent

    contains

        ! Agent constructor method
        subroutine init_ucb_agent (this, bandit_in, c_in)

            ! Declare variable
            class (ucb_agent), intent(inout) :: this

            type (multiarm_bandit), intent(in) :: bandit_in (:)
            real, intent(in) :: c_in

            ! Logger status
            integer :: return_stat


            ! Open logger file
            open(newunit=this%log_fd, file=trim(adjustl("./ucb.log")), status='NEW', action="WRITE", iostat=return_stat)
            if (return_stat /= 0) then
                error stop "Failed to open log file."
            end if

            ! Write header to log file
            write(unit=this%log_fd, fmt='(A,F0.10,A,I0)', iostat=return_stat)  "greedy_epsion, epsilon= ", epsilon_in, ", bandit_count= ", SIZE(bandit_in)
            write(unit=this%log_fd, fmt='(A)', iostat=return_stat)  ""
            write(unit=this%log_fd, fmt='(A)', iostat=return_stat)  "explore?, which_bandit, result"
            if (return_stat /= 0) then
                print *, "Failed to write to log file."
                stop
            end if

            ! Check and allocate memory for bandit instance
            if (allocated(this%bandit_instance)) deallocate(this%bandit_instance)
            allocate(this%bandit_instance(SIZE(bandit_in)))

            ! Set and assign agent's parameter
            this%bandit_instance = bandit_in
            this%c = c_in
            this%total_pull = 0


        end subroutine init_ucb_agent

        subroutine reset_ucb_agent(this)

            IMPLICIT NONE

            ! Declare variable
            class (ucb_agent), intent(inout) :: this

                        integer :: i
            integer :: return_stat

            ! Set all value to 0
            do i = 1, SIZE(this%bandit_instance)
                this%bandit_instance(i)%total_reward = 0
                this%bandit_instance(i)%pull_count = 0
                this%bandit_instance(i)%expected_reward = 0
            end do

            write(unit=this%log_fd, fmt='(A)', iostat=return_stat)  "Bandit parameter reset"
            if (return_stat /= 0) then
                print *, "Failed to write to log file."
                stop
            end if

        end subroutine reset_ucb_agent


        function iter_ucb_agent(this) result(n)
            IMPLICIT NONE

            ! Declare variable
            class(ucb_agent), intent(inout) :: this
            integer, intent(inout) :: n

            ! Internal variable
            integer :: choosen_instance
            real :: chosen_instance_score = -9e38
            real :: current_score

            integer :: i
            integer :: return_stat
            real :: rand_num

            ! Compute expected value + Hoeffding's inequality, then find max score
            do i = 1, SIZE(this%bandit_instance)
                arm_score(i) = this%bandit_instance

            end do

            ! Find which arm to pull

            ! Pull arm and update expected value

            ! Log value






        end function iter_ucb_agent


end module ucb_agent_module