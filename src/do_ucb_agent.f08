program main
    ! Import module
    use multiarm_bandit_module
    use ucb_agent_module

    ! Declare variable
    type(multiarm_bandit), dimension(10) :: bandit
    type(ucb_agent) :: agent_instance
    integer, dimension(2) :: reward = [0,1]
    integer, dimension(2,10) :: weight = reshape([91, 9, 61, 39, 1, 99, 68, 32, 84, 16, 65, 35, 11, 89, 99, 1, 23, 77, 98, 2], [2,10])
    integer :: i
    integer :: out


    ! Generate 10 multi-arm bandit instance, then assign with predefined reward and reward's weight
    do i = 1,10
        CALL set_reward(bandit(i), reward, weight(:, i))
    end do

    ! Initialized agent
    CALL init_ucb_agent(agent_instance, bandit, 5.0)

    ! Do 1000 iteration on c = 5.0 (Aggressive exploration)
    do i = 1, 500
        out = iter_ucb_agent(agent_instance)

    end do

    ! Do 9000 iteration on c = 1 (Less aggressive exploration)
    agent_instance%c = 1
    do i = 1, 9500
        out = iter_ucb_agent(agent_instance)

    end do


end program main