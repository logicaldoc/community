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
}