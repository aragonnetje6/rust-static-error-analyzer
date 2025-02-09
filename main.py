import matplotlib.pyplot as plt
import matplotlib
import pandas as pd

data = pd.read_csv("./data.csv")
print(data)
binaries = data[data["type"] == "binary"]
libraries = data[data["type"] == "library"]

plt.figure()
plt.scatter(
    libraries["LOC"],
    libraries["networks"],
    c="blue",
    label="libraries",
)
plt.scatter(
    binaries["LOC"],
    binaries["networks"],
    c="red",
    label="binaries",
)
plt.legend()
plt.title("Total panics")
plt.xlabel("Lines of code")
plt.ylabel("Number of detected panics")
plt.show()

plt.figure()
plt.scatter(
    libraries["LOC"],
    libraries["average"],
    c="blue",
    label="libraries",
)
plt.scatter(
    binaries["LOC"],
    binaries["average"],
    c="red",
    label="binaries",
)
plt.legend()
plt.title("Average panic network size")
plt.xlabel("Lines of code")
plt.ylabel("Average panic network size")
plt.show()

plt.figure()
plt.scatter(
    libraries["LOC"],
    libraries["largest"],
    c="blue",
    label="libraries",
)
plt.scatter(
    binaries["LOC"],
    binaries["largest"],
    c="red",
    label="binaries",
)
plt.legend()
plt.title("Largest panic network")
plt.xlabel("Lines of code")
plt.ylabel("Largest panic network size")
plt.show()

plt.figure()
plt.scatter(
    libraries["LOC"],
    libraries["networks"],
    c="blue",
    label="libraries",
)
plt.scatter(
    binaries["LOC"],
    binaries["networks"],
    c="red",
    label="binaries",
)
plt.legend()
plt.title("Number of panicking functions")
plt.xlabel("Lines of code")
plt.ylabel("Number of panics")
plt.show()

plt.figure()
plt.pie([binaries["invocations"].sum(), binaries["documented"].sum(), binaries["both"].sum()], labels=["", "", ""])
plt.title("Proportion of panic types in binary crates")
# plt.legend(loc=3, labels=["Explicitly invoked panics", "Documented panics", "Documented invocations"])
plt.show()

plt.figure()
plt.pie([libraries["invocations"].sum(), libraries["documented"].sum(), libraries["both"].sum()], labels=["", "", ""])
plt.title("Proportion of panic types in library crates")
plt.legend(loc=3, labels=["Explicitly invoked panics", "Documented panics", "Documented invocations"])
plt.show()
