from data import time_slots

# Define the fitness function
def fitness(schedule):
    total_fitness = 0.0

    # Initialize dictionaries to track facilitator load and room assignments
    facilitator_load = {}
    room_assignments = {}

    for activity in schedule:
        room_size = activity['room_size']
        expected_enrollment = activity['expected_enrollment']
        facilitator = activity['facilitator']
        time = activity['time']  # Corrected key

        # Check if the activity is scheduled at the same time in the same room as another activity
        if (time, room_size) in room_assignments:
            total_fitness -= 0.5

        # Room size constraints
        if room_size < expected_enrollment:
            total_fitness -= 0.5
        elif room_size > 3 * expected_enrollment:
            total_fitness -= 0.2
        elif room_size > 6 * expected_enrollment:
            total_fitness -= 0.4
        else:
            total_fitness += 0.3

        # Facilitator preferences
        if facilitator in activity['preferred_facilitators']:
            total_fitness += 0.5
        elif facilitator in activity['other_facilitators']:
            total_fitness += 0.2
        else:
            total_fitness -= 0.1

        # Update facilitator load
        if facilitator in facilitator_load:
            facilitator_load[facilitator].append(time)
        else:
            facilitator_load[facilitator] = [time]

    # Calculate facilitator load penalties
    for facilitator, times in facilitator_load.items():
        if len(times) == 1:
            total_fitness += 0.2
        elif len(times) > 1:
            total_fitness -= 0.2

        if len(times) > 4:
            total_fitness -= 0.5
        elif len(times) in (1, 2):
            if facilitator != "Dr. Tyler":
                total_fitness -= 0.4

    # Activity-specific adjustments
    for i in range(len(schedule)):
        activity = schedule[i]
        for j in range(i + 1, len(schedule)):
            other_activity = schedule[j]

            if (
                (activity['activity_name'].startswith('SLA101') or activity['activity_name'].startswith('SLA191')) and
                (other_activity['activity_name'].startswith('SLA101') or other_activity['activity_name'].startswith('SLA191'))
            ):
                if abs(time_slots.index(activity['time']) - time_slots.index(other_activity['time'])) > 4:
                    total_fitness += 0.5
                if activity['time'] == other_activity['time']:
                    total_fitness -= 0.5
                if (
                    activity['room'] in ['Roman 201', 'Beach 201'] or
                    other_activity['room'] in ['Roman 201', 'Beach 201']
                ):
                    if (
                        abs(time_slots.index(activity['time']) - time_slots.index(other_activity['time'])) == 1
                    ):
                        total_fitness += 0.25
                    if (
                        abs(time_slots.index(activity['time']) - time_slots.index(other_activity['time'])) == 1 and
                        (activity['room'] in ['Roman 201', 'Beach 201']) != (other_activity['room'] in ['Roman 201', 'Beach 201'])
                    ):
                        total_fitness -= 0.4
                if activity['time'] == other_activity['time']:
                    total_fitness -= 0.25

    return total_fitness
