    ! Do 10000 iteration
    do i = 1, 1000
        out = iter_y_agent(GY_agent)

    end do

    GY_agent%epsilon_value = 0.01

    do i = 1, 9000
        out = iter_y_agent(GY_agent)

    end do