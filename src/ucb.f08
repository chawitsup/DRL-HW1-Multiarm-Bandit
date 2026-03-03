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
        procedure :: init_ucb_agent
        procedure :: reset_ucb_agent
        procedure :: iter_ucb_agent
            
    end type ucb_agent

    contains

        ! Agent constructor method
        subroutine init_ucb_agent (this, bandit_in, c_in)
            IMPLICIT NONE
        
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
            write(unit=this%log_fd, fmt='(A,F0.10,A,I0)', iostat=return_stat)  "Upper Confedience Bounds, c= ", c_in, ", bandit_count= ", SIZE(bandit_in)
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
            integer :: n

            ! Internal variable
            integer :: choosen_instance = 1
            real :: choosen_instance_score = -9E37
            real :: score

            integer :: i
            integer :: return_stat
            real :: rand_num

            ! Compute arm with most expected value + Hoeffding's inequality
            do i = 1, SIZE(this%bandit_instance)

                ! If any bandit_instance is never take, force take it
                if (this%bandit_instance(i)%pull_count == 0) then
                    choosen_instance = i
                    exit
                end if

                ! Compute score (expected value + Hoeffding's inequality)
                score = LOG(real(this%total_pull)) / real(this%bandit_instance(i)%pull_count)
                score = this%c * SQRT(score)
                score = score + this%bandit_instance(i)%expected_reward

                ! Choose instance with max score
                if (score > choosen_instance_score) then

                    choosen_instance = i
                    choosen_instance_score = score

                end if

            end do

            ! Pull arm and update expected value
            n = pull(this%bandit_instance(choosen_instance))
            this%bandit_instance(choosen_instance)%total_reward = this%bandit_instance(choosen_instance)%total_reward + n
            this%bandit_instance(choosen_instance)%pull_count = this%bandit_instance(choosen_instance)%pull_count + 1
            this%bandit_instance(choosen_instance)%expected_reward = real(this%bandit_instance(choosen_instance)%total_reward) / real(this%bandit_instance(choosen_instance)%pull_count)

            ! Iterate to next agent's timestep
            this%total_pull = this%total_pull + 1

            ! Log value
            write(unit=this%log_fd, fmt='(A,I0,A,I0)', iostat=return_stat, ADVANCE='NO')  "?, ", choosen_instance,", ", n

            ! Dump all expected value
            do i = 1, SIZE(this%bandit_instance)
            write(unit=this%log_fd, fmt='(A,F10.8)', iostat=return_stat, ADVANCE='NO') ", ", this%bandit_instance(i)%expected_reward

            end do 

            ! New line
            write(unit=this%log_fd, fmt='(A)', iostat=return_stat, ADVANCE='YES') ""
            
            if (return_stat /= 0) then
                print *, "Failed to write to log file."
                stop
            end if


        end function iter_ucb_agent


end module ucb_agent_module