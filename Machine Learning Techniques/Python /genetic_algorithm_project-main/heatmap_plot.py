import seaborn as sns
import matplotlib.pyplot as plt


def generate_heatplot(data, x_labels, y_labels, output_file=None):

    # Create a larger figure to add space on the left
    fig, ax = plt.subplots(figsize=(8, 6))
    plt.subplots_adjust(left=0.2)

    # Create a heatmap using seaborn
    sns.set(font_scale=1.0)
    sns.heatmap(data, annot=True, fmt="d", cmap='YlGnBu', xticklabels=x_labels, yticklabels=y_labels)


    # Axis labels and title
    plt.xlabel('Time Slots')
    plt.ylabel('Rooms')
    plt.title('Room Occupancy Heatmap')

    # If an output file is provided, save the heatmap as an image
    if output_file:
        plt.savefig(output_file)
    else:
        # Show the heatmap if no output file is specified
        plt.show()

