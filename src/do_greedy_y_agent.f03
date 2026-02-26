program main
    ! Import module
    use multiarm_bandit_module
    use greedy_y_agent_module

    ! Declare variable
    type(multiarm_bandit), dimension(10) :: bandit
    type(greedy_y_agent) :: GY_agent
    integer, dimension(2) :: reward = [0,1]
    integer, dimension(2,10) :: weight = reshape([91, 9, 61, 39, 1, 99, 68, 32, 84, 16, 65, 35, 11, 89, 99, 1, 23, 77, 98, 2], [2,10])
    integer :: i
    integer :: out


    ! Generate 10 multi-arm bandit instance, then assign with predefined reward and reward's weight
    do i = 1,10
        CALL set_reward(bandit(i), reward, weight(:, i))
    end do

    ! Initialized agent
    CALL init_y_agent(GY_agent, bandit, 0.3)

    ! Do 1000 iteration
    do i = 1, 9999
        out = iter_y_agent(GY_agent)

    end do




end program main