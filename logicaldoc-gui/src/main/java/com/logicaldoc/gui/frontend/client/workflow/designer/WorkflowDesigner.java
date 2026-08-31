package com.logicaldoc.gui.frontend.client.workflow.designer;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.logicaldoc.gui.common.client.beans.GUITransition;
import com.logicaldoc.gui.common.client.beans.GUIWFState;
import com.logicaldoc.gui.common.client.beans.GUIWorkflow;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.orange.links.client.connection.Connection;
import com.orange.links.client.shapes.DrawableSet;
import com.orange.links.client.shapes.FunctionShape;
import com.orange.links.client.shapes.Point;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Where the workflow diagram is drawn
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class WorkflowDesigner extends AdminPanel {

	public static final int TYPE_TASK = 0;

	public static final int TYPE_END = 1;

	public static final int TYPE_JOIN = 2;

	public static final int TYPE_FORK = 3;

	private VLayout drawingPanelLayout = new VLayout();

	private GUIWorkflow workflow = null;

	private WorkflowDrawingPanel drawingPanel = null;

	private WorkflowToolStrip workflowToolstrip;

	public WorkflowDesigner(GUIWorkflow workflow) {
		super("workflow");

		this.workflow = workflow;

		drawingPanelLayout.setWidth100();
		drawingPanelLayout.setHeight100();

		PrimitivesToolstrip primitives = new PrimitivesToolstrip(this);
		workflowToolstrip = new WorkflowToolStrip(this, primitives);

		drawingPanelLayout.addMember(workflowToolstrip);
		drawingPanelLayout.addMember(primitives);

		drawingPanel = new WorkflowDrawingPanel(this);
		drawingPanelLayout.addMember(drawingPanel);

		body.setMembers(drawingPanelLayout);

		if (workflow != null)
			redraw(workflow);
	}

	public GUIWorkflow getWorkflow() {
		return workflow;
	}

	public void redraw(GUIWorkflow workflow) {
		this.workflow = workflow;
		this.workflowToolstrip.setCurrentWorkflow(workflow);
		drawingPanel.redraw();
	}

	public void refresh() {
		for (GUIWFState state : workflow.getStates()) {
			StateWidget widget = getDrawingPanel().getWidget(state.getId());
			if (widget != null && widget.isTask())
				widget.update();
		}
		getDrawingPanel().redraw();
	}

	public void onAddState(int type) {
		GUIWFState state = new GUIWFState("" + new Date().getTime(), I18N.message("statename"), type);
		getWorkflow().getStates().add(state);

		/*
		 * Check if this must be the initial state
		 */
		if (type == GUIWFState.TYPE_TASK) {
			state.setInitial(true);
			if (getWorkflow().getStates() != null)
				for (GUIWFState s : getWorkflow().getStates()) {
					if (s.getType() == GUIWFState.TYPE_TASK && s.isInitial()) {
						state.setInitial(false);
						break;
					}
				}
		}

		if (state.isInitial())
			getWorkflow().setStartStateId(state.getId());

		StateWidget sw = new StateWidget(drawingPanel, state);

		int x = (drawingPanel.getRect().getWidth() - sw.getWidth()) / 2 + drawingPanel.getScrollLeft();
		int y = (drawingPanel.getRect().getHeight() - sw.getHeight()) / 2 + drawingPanel.getScrollTop();
		drawingPanel.getDiagramController().addWidget(sw, x, y);
		drawingPanel.getDiagramController().makeDraggable(sw);

		try {
			saveModel();
		} catch (Exception t) {
			// Nothing to do
		}
	}

	public WorkflowDrawingPanel getDrawingPanel() {
		return drawingPanel;
	}

	/**
	 * Saves the current diagram into the object model.
	 */
	public void saveModel() {
		// Collect all the states as drawn in the designer.
		int i = 0;

		workflow.getStates().clear();
		for (FunctionShape shape : getDrawingPanel().getDiagramController().getShapes()) {
			StateWidget widget = (StateWidget) shape.getWidget();

			String id = Integer.toString(i++);

			GUIWFState wfState = widget.getWFState();
			if (wfState.getId().equals(workflow.getStartStateId())) {
				workflow.setStartStateId(id);
				wfState.setInitial(true);
			} else
				wfState.setInitial(false);
			wfState.setId(id);
			wfState.setTop(shape.getTop());
			wfState.setLeft(shape.getLeft());
			workflow.getStates().add(wfState);
		}

		// Collect all the transitions as drawn in the designer
		for (FunctionShape shape : getDrawingPanel().getDiagramController().getShapes()) {
			StateWidget srcWidget = (StateWidget) shape.getWidget();

			DrawableSet<Connection> connections = shape.getConnections();
			List<GUITransition> transitions = new ArrayList<>();
			for (Connection connection : connections) {
				StateWidget start = (StateWidget) ((FunctionShape) connection.getStartShape()).getWidget();
				if (!start.equals(srcWidget))
					continue;

				StateWidget end = (StateWidget) ((FunctionShape) connection.getEndShape()).getWidget();
				GUITransition transition = ((StateWidget) connection.getDecoration().getWidget()).getTransition();
				transition.setTargetState(end.getWFState());
				transitions.add(transition);
				StringBuilder sb = new StringBuilder("");
				for (Point point : connection.getMovablePoints()) {
					sb.append("" + point.getLeft());
					sb.append("," + point.getTop());
					sb.append(";");
				}

				transition.setPoints(sb.toString());
			}

			srcWidget.getWFState().setTransitions(transitions);
		}
	}
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}