package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * Representation of a workflow transition.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class GUITransition implements Serializable {

	private static final long serialVersionUID = 1L;

	private String text;

	private String color;

	private GUIWFState targetState;

	private String points;

	private String onChosen;

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

	public GUITransition() {
	}

	public GUITransition(String text) {
		this.text = text;
	}

	public GUITransition(String text, String color) {
		this.text = text;
		this.color = color;
	}

	public GUITransition(String text, GUIWFState targetState) {
		this.text = text;
		this.targetState = targetState;
	}

	public String getText() {
		return text;
	}

	public void setText(String text) {
		this.text = text;
	}

	public GUIWFState getTargetState() {
		return targetState;
	}

	public void setTargetState(GUIWFState targetState) {
		this.targetState = targetState;
	}

	public String getPoints() {
		return points;
	}

	public void setPoints(String points) {
		this.points = points;
	}

	public String getOnChosen() {
		return onChosen;
	}

	public void setOnChosen(String onChosen) {
		this.onChosen = onChosen;
	}

	public String getColor() {
		return color;
	}

	public void setColor(String color) {
		this.color = color;
	}

	public boolean isRequiresNote() {
		return requiresNote;
	}

	public Integer getMinNoteSize() {
		return minNoteSize;
	}

	public void setRequiresNote(boolean requiresNote) {
		this.requiresNote = requiresNote;
	}

	public void setMinNoteSize(Integer minNoteSize) {
		this.minNoteSize = minNoteSize;
	}

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((color == null) ? 0 : color.hashCode());
        result = prime * result + ((minNoteSize == null) ? 0 : minNoteSize.hashCode());
        result = prime * result + ((onChosen == null) ? 0 : onChosen.hashCode());
        result = prime * result + ((points == null) ? 0 : points.hashCode());
        result = prime * result + (requiresNote ? 1231 : 1237);
        result = prime * result + ((targetState == null) ? 0 : targetState.hashCode());
        result = prime * result + ((text == null) ? 0 : text.hashCode());
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
        GUITransition other = (GUITransition) obj;
        if (color == null) {
            if (other.color != null)
                return false;
        } else if (!color.equals(other.color))
            return false;
        if (minNoteSize == null) {
            if (other.minNoteSize != null)
                return false;
        } else if (!minNoteSize.equals(other.minNoteSize))
            return false;
        if (onChosen == null) {
            if (other.onChosen != null)
                return false;
        } else if (!onChosen.equals(other.onChosen))
            return false;
        if (points == null) {
            if (other.points != null)
                return false;
        } else if (!points.equals(other.points))
            return false;
        if (requiresNote != other.requiresNote)
            return false;
        if (targetState == null) {
            if (other.targetState != null)
                return false;
        } else if (!targetState.equals(other.targetState))
            return false;
        if (text == null) {
            if (other.text != null)
                return false;
        } else if (!text.equals(other.text))
            return false;
        return true;
    }
}