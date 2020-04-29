---
# Render this document using the following command":
#
#    pandoc -s wtp.md --pdf-engine=xelatex -o wtp.pdf
#
title: "Web Testing Platform"
author: "Oskar Wickström"
---

...

## Deleting Drafts

In this example, we have a list of drafts that can be deleted. The user
selects one or more drafts, by checking the corresponding checkboxes,
and clicks "Delete". A confirmation dialog is shown, and the user can
either cancel or confirm the deletion.

 We want to show that when drafts are selected for deletion and the user has
 clicked "Delete", entering the $\textit{confirming}$ state, the deletion is
 either:

1. _cancelled_, meaning that no drafts are deleted, the same set of drafts 
   are selected, and the confirmation dialog is hidden, or
2. _confirmed_, meaning that the set of selected drafts are deleted from
   the drafts list and that the confirmation dialog is hidden

These are the only two valid actions when in the $\textit{confirming}$ state.

The following formula defines the $\textit{confirming}$ state as the
existence of an element $e$ returned by querying the current DOM for the CSS
selector $\mathtt{confirm}$, that is visible and has the text content "Are
you sure?".

\begin{equation}
\mathit{confirming} = \exists e \in \mathit{query}(\mathtt{.confirm}) : \mathit{e}.\mathrm{visible} \wedge \mathit{e}.\mathrm{text} = \text{"Are you sure?"} \\
\end{equation}

We also need a version of $\mathit{confirming}$ that instead refers to
the next state. This is done using the primed querying operator
$\mathit{query'}$.

\begin{equation}
\mathit{confirmingNext} = \exists e \in \mathit{query'}(\mathtt{.confirm}) : \mathit{e}.\mathrm{visible} \wedge \mathit{e}.\mathrm{text} = \text{"Are you sure?"} \\
\end{equation}

We can now define the $\mathit{cancel}$ action. It says that the set of
drafts (or their checkboxes, rather) are the same in the current and
next state, that the same checkboxes are checked, and that we're no
longer $\mathit{confirming}$ in the next state.

\begin{equation}
\begin{aligned}
\mathit{cancel} =\ & \mathit{query}(\mathtt{.checkbox}) = \mathit{query'}(\mathtt{.checkbox}) \\
  & \wedge \{c \in \mathit{query}(\mathtt{.checkbox}) : \mathit{c}\mathit{.checked} \} = \{c \in \mathit{query'}(\mathtt{.checkbox}) : \mathit{c}\mathit{.checked} \} \\
  & \wedge \lnot \mathit{confirmingNext} \\
\end{aligned}
\end{equation}

The $\mathit{confirm}$ action is the other possibility. It says that the
resulting set of checkboxes is equal to the currently non-checked ones,
and that we're no longer $\mathit{confirming}$ in the next state.

\begin{equation}
\begin{aligned}
\mathit{confirm} =\ & \{c \in \mathit{query}(\mathtt{.checkbox}) : \lnot \mathit{c}\mathit{.checked} \} = \mathit{query'}(\mathtt{.checkbox}) \\
  & \wedge \lnot \mathit{confirmingNext} \\
\end{aligned}
\end{equation}

Finally, we can compose our building blocks to define the safety property. At
all times ($\square$), when we're confirming the deletion of selected drafts,
we can either $\mathit{cancel}$ or $\mathit{confirm}$.

\begin{equation}
\square (\mathit{confirming} \implies \mathit{cancel} \vee \mathit{confirm})
\end{equation}

That's it.

## Reading material

- [LTL patterns survey](http://santos.cs.ksu.edu/esscass04/papers/patterns-survey.pdf)
- [Intro to TLA](https://lamport.azurewebsites.net/pubs/intro-to-tla.pdf)
- [Specifiying Concurrent Systems with TLA+](https://www.microsoft.com/en-us/research/uploads/prod/2016/12/Specifying-Concurrent-Systems-with-TLA.pdf)
