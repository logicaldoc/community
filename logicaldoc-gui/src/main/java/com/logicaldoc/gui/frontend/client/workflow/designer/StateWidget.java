package com.logicaldoc.gui.frontend.client.workflow.designer;

import com.logicaldoc.gui.common.client.beans.GUITransition;
import com.logicaldoc.gui.common.client.beans.GUIWFState;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.LD;
import com.orange.links.client.DiagramController;
import com.orange.links.client.connection.Connection;
import com.orange.links.client.shapes.FunctionShape;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.DoubleClickEvent;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;

/**
 * Base visual representation of a Workflow object (a state or a transition).
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.3
 */
public class StateWidget extends Label {

	private GUIWFState wfState;

	private GUITransition transition;

	private WorkflowDrawingPanel drawingPanel;

	private DiagramController diagramController;

	private Connection connection;

	private boolean readonly = false;

	public StateWidget(Connection connection, DiagramController diagramController, GUITransition trans) {
		super(trans.getColor() != null ? "<span style='color:" + trans.getColor() + "'>" + trans.getText() + "</span>"
				: trans.getText());

		String name = trans.getText();
		if (name != null) {
			name = name.replace("<b>", "");
			name = name.replace("</b>", "");
			name = name.replace("&nbsp;", "");
		}

		this.connection = connection;
		this.diagramController = diagramController;
		this.transition = trans;
		transition.setText(name);
		setAutoFit(true);
		setAutoHeight();

		addDoubleClickHandler((DoubleClickEvent event) -> {
			if (readonly)
				return;

			onDoubleClick();

			event.cancel();
		});
	}

	private void onDoubleClick() {
		Menu contextMenu = new Menu();

		MenuItem edit = new MenuItem();
		edit.setTitle(I18N.message("edit"));
		edit.addClickHandler(click -> edit());

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(
				click -> LD.ask(I18N.message("ddelete"), I18N.message("confirmdelete"), (Boolean answer) -> {
					if (Boolean.TRUE.equals(answer))
						delete();
				}));

		MenuItem makeStart = new MenuItem();
		makeStart.setTitle(I18N.message("startstate"));
		makeStart.addClickHandler(click -> makeStartState());

		MenuItem straight = new MenuItem();
		straight.setTitle(I18N.message("straight"));
		straight.addClickHandler(click -> restoreStraight());

		if (isTask())
			contextMenu.setItems(edit, makeStart, delete);
		else if (isConnection()) {
			StateWidget start = (StateWidget) ((FunctionShape) connection.getStartShape()).getWidget();
			if (start.isFork() || start.isJoin())
				// No edit in case the transition starts from a fork or join
				// node
				contextMenu.setItems(straight, new MenuItemSeparator(), delete);
			else
				contextMenu.setItems(edit, straight, new MenuItemSeparator(), delete);
		} else
			contextMenu.setItems(edit, delete);
		contextMenu.showContextMenu();
	}

	/**
	 * Constructor used by new transitions
	 * 
	 * @param connection the connection
	 * @param diagramController the controller
	 * @param name the name of the widget
	 * @param color the color
	 */
	public StateWidget(Connection connection, DiagramController diagramController, String name, String color) {
		this(connection, diagramController, new GUITransition(name, color));
	}

	public StateWidget(WorkflowDrawingPanel dp, GUIWFState state) {
		this(null, dp.getDiagramController(), "<b>" + state.getName() + "</b>&nbsp;", null);
		this.wfState = state;
		this.drawingPanel = dp;
		this.diagramController = dp.getDiagramController();

		update();
	}

	public void update() {
		if (wfState == null)
			return;

		setPadding(4);
		setMargin(3);
		setWrap(false);
		setAlign(Alignment.CENTER);
		setValign(VerticalAlignment.CENTER);
		setHeight(40);
		setAutoWidth();
		setOpacity(100);
		setBackgroundColor("#FFFFFF");
		setContents("<b>" + wfState.getName() + "</b>&nbsp;");

		int type = wfState.getType();
		if (type == GUIWFState.TYPE_END) {
			setIcon("[SKIN]/endState.png");
			setBorder("3px solid #444444");
		} else if (type == GUIWFState.TYPE_TASK) {
			setIcon("[SKIN]/task.png");
			if (isStartState())
				setBorder("3px solid #15F219");
			else
				setBorder("3px solid #2281D0");
		} else if (type == GUIWFState.TYPE_JOIN) {
			setIcon("[SKIN]/join.png");
			setBorder("3px solid #F2EE07");
		} else if (type == GUIWFState.TYPE_FORK) {
			setIcon("[SKIN]/fork.png");
			setBorder("3px solid #F2EE07");
		} else {
			setAutoFit(true);
			setAutoHeight();
		}

		if (wfState.getColor() != null)
			setBorder("3px solid " + wfState.getColor());
	}

	private void edit() {
		if (isTask() || isEnd()) {
			new TaskEditor(StateWidget.this).show();
		} else if (isJoin() || isFork()) {
			new ForkEditor(StateWidget.this).show();
		} else {
			new TransitionEditor(StateWidget.this).show();
		}
	}

	private void delete() {
		if (wfState != null) {
			diagramController.deleteWidget(this);
		} else {
			connection.getStartShape().removeConnection(connection);
			diagramController.deleteConnection(connection);
		}
	}

	private void makeStartState() {
		WorkflowDesigner workflowDesigner = getDrawingPanel().getWorkflowDesigner();
		try {
			workflowDesigner.saveModel();
		} catch (Exception t) {
			// Nothing to do
		}

		workflowDesigner.getWorkflow().setStartStateId(wfState.getId());
		for (GUIWFState s : workflowDesigner.getWorkflow().getStates()) {
			s.setInitial(false);
		}
		getWFState().setInitial(true);
		workflowDesigner.saveModel();
		workflowDesigner.refresh();
	}

	public boolean isEnd() {
		return wfState != null && wfState.getType() == GUIWFState.TYPE_END;
	}

	public boolean isTask() {
		return wfState != null && wfState.getType() == GUIWFState.TYPE_TASK;
	}

	public boolean isJoin() {
		return wfState != null && wfState.getType() == GUIWFState.TYPE_JOIN;
	}

	public boolean isFork() {
		return wfState != null && wfState.getType() == GUIWFState.TYPE_FORK;
	}

	public boolean isStartState() {
		if (wfState != null)
			return wfState.isInitial();
		else
			return false;
	}

	public boolean isConnection() {
		return connection != null;
	}

	public GUIWFState getWFState() {
		return wfState;
	}

	public WorkflowDrawingPanel getDrawingPanel() {
		return drawingPanel;
	}

	public DiagramController getDiagramController() {
		return diagramController;
	}

	public void setDiagramController(DiagramController diagramController) {
		this.diagramController = diagramController;
	}

	public Connection getConnection() {
		return connection;
	}

	public void setConnection(Connection connection) {
		this.connection = connection;
	}

	public boolean isReadonly() {
		return readonly;
	}

	public void setReadonly(boolean readonly) {
		this.readonly = readonly;
	}

	/**
	 * Restore the connection to a straight segment
	 */
	private void restoreStraight() {
		connection.getMovablePoints().clear();
		connection.setStraight();
		diagramController.runRefresh();
	}

	public GUITransition getTransition() {
		return transition;
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