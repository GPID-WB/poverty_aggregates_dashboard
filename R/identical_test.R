



# Count how many are exactly the same (non-NA values only)
class_data %>%
    filter(!is.na(incgroup_historical) & !is.na(incgroup_current)) %>%
    mutate(same = incgroup_historical == incgroup_current) %>%
    count(same)


# Count how many are exactly the same (non-NA values only)
class_data %>%
    filter(!is.na(fcv_historical) & !is.na(fcv_current)) %>%
    mutate(same = fcv_historical == fcv_current) %>%
    count(same)


class_data %>%
    filter(!is.na(ida_historical) & !is.na(ida_current)) %>%
    mutate(same = ida_historical == ida_current) %>%
    count(same)
