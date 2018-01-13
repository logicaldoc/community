package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * Representation of a workflow transition.
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 6.0
 */
public class GUITransition implements Serializable {

	private static final long serialVersionUID = 1L;

	private String text;

	private GUIWFState targetState;

	private String points;

	private String onChosen;

	public GUITransition() {
	}

	public GUITransition(String text) {
		this.text = text;
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
}