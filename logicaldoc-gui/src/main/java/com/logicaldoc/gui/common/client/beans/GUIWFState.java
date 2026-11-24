package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.logicaldoc.gui.common.client.Constants;

/**
 * Workflow State bean as used in the GUI
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class GUIWFState implements Serializable {

    private static final long serialVersionUID = 1L;

    public static final int TYPE_UNDEFINED = -1;

    public static final int TYPE_TASK = 0;

    public static final int TYPE_END = 1;

    public static final int TYPE_JOIN = 2;

    public static final int TYPE_FORK = 3;

    private int type = TYPE_TASK;

    private String id;

    private String name;

    private String description;

    private int dueDateNumber = 0;

    private String dueDateUnit = Constants.TIME_MINUTE;

    private int reminderNumber = 0;

    private String reminderUnit = Constants.TIME_MINUTE;

    private String owner = "";

    private String pooledActors = "";

    private Date startDate;

    private Date endDate;

    private Date dueDate;

    private String taskState;

    private int top = 0;

    private int left = 0;

    private boolean initial = false;

    private String onCreation;

    private String onAssignment;

    private String onCompletion;

    private String onOverdue;

    private String onValidation;

    private String display;

    private String color;

    private String creationMessageTemplate;

    private String completionMessageTemplate;

    private String assignmentMessageTemplate;

    private String reminderMessageTemplate;

    /**
     * If true it means that the user must enter a note when the he completes
     * the task
     */
    private boolean requiresNote = false;

    /**
     * If the requiredNote=1 this indicates an optional minimum size of the note
     * text
     */
    private Integer minNoteSize;

    private List<GUIValue> candidates = new ArrayList<>();

    private List<GUITransition> transitions = new ArrayList<>();

    public GUIWFState() {
    }

    public GUIWFState(String id, String name, int type) {
        this.id = id;
        this.name = name;
        this.type = type;
    }

    public String getOnOverdue() {
        return onOverdue;
    }

    public void setOnOverdue(String onOverdue) {
        this.onOverdue = onOverdue;
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getIcon() {
        if (type == TYPE_TASK) {
            return "[SKIN]/task.png";
        } else if (type == TYPE_JOIN) {
            return "[SKIN]/join.png";
        } else if (type == TYPE_FORK) {
            return "[SKIN]/fork.png";
        } else if (type == TYPE_END) {
            return "[SKIN]/endState.png";
        } else {
            return "";
        }
    }

    public int getDueDateNumber() {
        return dueDateNumber;
    }

    public void setDueDateNumber(int dueDateNumber) {
        this.dueDateNumber = dueDateNumber;
    }

    public String getDueDateUnit() {
        return dueDateUnit;
    }

    public void setDueDateUnit(String dueDateUnit) {
        this.dueDateUnit = dueDateUnit;
    }

    public int getReminderNumber() {
        return reminderNumber;
    }

    public void setReminderNumber(int reminderNumber) {
        this.reminderNumber = reminderNumber;
    }

    public String getReminderUnit() {
        return reminderUnit;
    }

    public void setReminderUnit(String reminderUnit) {
        this.reminderUnit = reminderUnit;
    }

    public String getOwner() {
        return owner;
    }

    public void setOwner(String owner) {
        this.owner = owner;
    }

    public Date getStartDate() {
        return startDate;
    }

    public void setStartDate(Date startDate) {
        this.startDate = startDate;
    }

    public Date getEndDate() {
        return endDate;
    }

    public void setEndDate(Date endDate) {
        this.endDate = endDate;
    }

    public String getTaskState() {
        return taskState;
    }

    public void setTaskState(String taskState) {
        this.taskState = taskState;
    }

    public String getPooledActors() {
        return pooledActors;
    }

    public void setPooledActors(String pooledActors) {
        this.pooledActors = pooledActors;
    }

    public int getTop() {
        return top;
    }

    public void setTop(int top) {
        this.top = top;
    }

    public int getLeft() {
        return left;
    }

    public void setLeft(int left) {
        this.left = left;
    }

    public boolean isInitial() {
        return initial;
    }

    public void setInitial(boolean initial) {
        this.initial = initial;
    }

    public Date getDueDate() {
        return dueDate;
    }

    public void setDueDate(Date dueDate) {
        this.dueDate = dueDate;
    }

    public String getOnCreation() {
        return onCreation;
    }

    public void setOnCreation(String onCreation) {
        this.onCreation = onCreation;
    }

    public String getOnAssignment() {
        return onAssignment;
    }

    public void setOnAssignment(String onAssignment) {
        this.onAssignment = onAssignment;
    }

    public String getOnCompletion() {
        return onCompletion;
    }

    public void setOnCompletion(String onCompletion) {
        this.onCompletion = onCompletion;
    }

    public String getDisplay() {
        return display;
    }

    public void setDisplay(String display) {
        this.display = display;
    }

    public String getColor() {
        return color;
    }

    public void setColor(String color) {
        this.color = color;
    }

    public String getCreationMessageTemplate() {
        return creationMessageTemplate;
    }

    public String getAssignmentMessageTemplate() {
        return assignmentMessageTemplate;
    }

    public String getReminderMessageTemplate() {
        return reminderMessageTemplate;
    }

    public void setCreationMessageTemplate(String creationMessageTemplate) {
        this.creationMessageTemplate = creationMessageTemplate;
    }

    public void setAssignmentMessageTemplate(String assignmentMessageTemplate) {
        this.assignmentMessageTemplate = assignmentMessageTemplate;
    }

    public void setReminderMessageTemplate(String reminderMessageTemplate) {
        this.reminderMessageTemplate = reminderMessageTemplate;
    }

    public String getCompletionMessageTemplate() {
        return completionMessageTemplate;
    }

    public void setCompletionMessageTemplate(String completionMessageTemplate) {
        this.completionMessageTemplate = completionMessageTemplate;
    }

    public boolean isRequiresNote() {
        return requiresNote;
    }

    public void setRequiresNote(boolean requiresNote) {
        this.requiresNote = requiresNote;
    }

    public Integer getMinNoteSize() {
        return minNoteSize;
    }

    public void setMinNoteSize(Integer minNoteSize) {
        this.minNoteSize = minNoteSize;
    }

    public List<GUIValue> getCandidates() {
        return candidates;
    }

    public void setCandidates(List<GUIValue> candidates) {
        this.candidates = candidates;
    }

    public List<GUITransition> getTransitions() {
        return transitions;
    }

    public void setTransitions(List<GUITransition> transitions) {
        this.transitions = transitions;
    }

    public String getOnValidation() {
        return onValidation;
    }

    public void setOnValidation(String onValidation) {
        this.onValidation = onValidation;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((assignmentMessageTemplate == null) ? 0 : assignmentMessageTemplate.hashCode());
        result = prime * result + ((candidates == null) ? 0 : candidates.hashCode());
        result = prime * result + ((color == null) ? 0 : color.hashCode());
        result = prime * result + ((completionMessageTemplate == null) ? 0 : completionMessageTemplate.hashCode());
        result = prime * result + ((creationMessageTemplate == null) ? 0 : creationMessageTemplate.hashCode());
        result = prime * result + ((description == null) ? 0 : description.hashCode());
        result = prime * result + ((display == null) ? 0 : display.hashCode());
        result = prime * result + ((dueDate == null) ? 0 : dueDate.hashCode());
        result = prime * result + dueDateNumber;
        result = prime * result + ((dueDateUnit == null) ? 0 : dueDateUnit.hashCode());
        result = prime * result + ((endDate == null) ? 0 : endDate.hashCode());
        result = prime * result + (initial ? 1231 : 1237);
        result = prime * result + left;
        result = prime * result + ((minNoteSize == null) ? 0 : minNoteSize.hashCode());
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        result = prime * result + ((onAssignment == null) ? 0 : onAssignment.hashCode());
        result = prime * result + ((onCompletion == null) ? 0 : onCompletion.hashCode());
        result = prime * result + ((onCreation == null) ? 0 : onCreation.hashCode());
        result = prime * result + ((onOverdue == null) ? 0 : onOverdue.hashCode());
        result = prime * result + ((onValidation == null) ? 0 : onValidation.hashCode());
        result = prime * result + ((owner == null) ? 0 : owner.hashCode());
        result = prime * result + ((pooledActors == null) ? 0 : pooledActors.hashCode());
        result = prime * result + ((reminderMessageTemplate == null) ? 0 : reminderMessageTemplate.hashCode());
        result = prime * result + reminderNumber;
        result = prime * result + ((reminderUnit == null) ? 0 : reminderUnit.hashCode());
        result = prime * result + (requiresNote ? 1231 : 1237);
        result = prime * result + ((startDate == null) ? 0 : startDate.hashCode());
        result = prime * result + ((taskState == null) ? 0 : taskState.hashCode());
        result = prime * result + top;
        result = prime * result + ((transitions == null) ? 0 : transitions.hashCode());
        result = prime * result + type;
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        GUIWFState other = (GUIWFState) obj;
        if (assignmentMessageTemplate == null) {
            if (other.assignmentMessageTemplate != null)
                return false;
        } else if (!assignmentMessageTemplate.equals(other.assignmentMessageTemplate))
            return false;
        if (candidates == null) {
            if (other.candidates != null)
                return false;
        } else if (!candidates.equals(other.candidates))
            return false;
        if (color == null) {
            if (other.color != null)
                return false;
        } else if (!color.equals(other.color))
            return false;
        if (completionMessageTemplate == null) {
            if (other.completionMessageTemplate != null)
                return false;
        } else if (!completionMessageTemplate.equals(other.completionMessageTemplate))
            return false;
        if (creationMessageTemplate == null) {
            if (other.creationMessageTemplate != null)
                return false;
        } else if (!creationMessageTemplate.equals(other.creationMessageTemplate))
            return false;
        if (description == null) {
            if (other.description != null)
                return false;
        } else if (!description.equals(other.description))
            return false;
        if (display == null) {
            if (other.display != null)
                return false;
        } else if (!display.equals(other.display))
            return false;
        if (dueDate == null) {
            if (other.dueDate != null)
                return false;
        } else if (!dueDate.equals(other.dueDate))
            return false;
        if (dueDateNumber != other.dueDateNumber)
            return false;
        if (dueDateUnit == null) {
            if (other.dueDateUnit != null)
                return false;
        } else if (!dueDateUnit.equals(other.dueDateUnit))
            return false;
        if (endDate == null) {
            if (other.endDate != null)
                return false;
        } else if (!endDate.equals(other.endDate))
            return false;
        if (initial != other.initial)
            return false;
        if (left != other.left)
            return false;
        if (minNoteSize == null) {
            if (other.minNoteSize != null)
                return false;
        } else if (!minNoteSize.equals(other.minNoteSize))
            return false;
        if (name == null) {
            if (other.name != null)
                return false;
        } else if (!name.equals(other.name))
            return false;
        if (onAssignment == null) {
            if (other.onAssignment != null)
                return false;
        } else if (!onAssignment.equals(other.onAssignment))
            return false;
        if (onCompletion == null) {
            if (other.onCompletion != null)
                return false;
        } else if (!onCompletion.equals(other.onCompletion))
            return false;
        if (onCreation == null) {
            if (other.onCreation != null)
                return false;
        } else if (!onCreation.equals(other.onCreation))
            return false;
        if (onOverdue == null) {
            if (other.onOverdue != null)
                return false;
        } else if (!onOverdue.equals(other.onOverdue))
            return false;
        if (onValidation == null) {
            if (other.onValidation != null)
                return false;
        } else if (!onValidation.equals(other.onValidation))
            return false;
        if (owner == null) {
            if (other.owner != null)
                return false;
        } else if (!owner.equals(other.owner))
            return false;
        if (pooledActors == null) {
            if (other.pooledActors != null)
                return false;
        } else if (!pooledActors.equals(other.pooledActors))
            return false;
        if (reminderMessageTemplate == null) {
            if (other.reminderMessageTemplate != null)
                return false;
        } else if (!reminderMessageTemplate.equals(other.reminderMessageTemplate))
            return false;
        if (reminderNumber != other.reminderNumber)
            return false;
        if (reminderUnit == null) {
            if (other.reminderUnit != null)
                return false;
        } else if (!reminderUnit.equals(other.reminderUnit))
            return false;
        if (requiresNote != other.requiresNote)
            return false;
        if (startDate == null) {
            if (other.startDate != null)
                return false;
        } else if (!startDate.equals(other.startDate))
            return false;
        if (taskState == null) {
            if (other.taskState != null)
                return false;
        } else if (!taskState.equals(other.taskState))
            return false;
        if (top != other.top)
            return false;
        return type == other.type;
    }
}