package com.logicaldoc.gui.frontend.client.workflow;

import com.logicaldoc.gui.common.client.beans.GUITransition;
import com.logicaldoc.gui.common.client.beans.GUIWFState;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.LD;
import com.orange.links.client.DiagramController;
import com.orange.links.client.connection.Connection;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.DoubleClickEvent;
import com.smartgwt.client.widgets.events.DoubleClickHandler;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * Base visual representation of a Workflow object (a state or a transition).
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.3
 */
public class StateWidget extends Label {

	private GUIWFState wfState;

	private GUITransition transition;

	private DrawingPanel drawingPanel;

	private DiagramController diagramController;

	private Connection connection;

	private boolean readonly = false;

	public StateWidget(Connection connection, DiagramController diagramController, GUITransition trans) {
		super(trans.getText());

		String name = trans.getText();
		if (name != null) {
			name = name.replaceAll("<b>", "");
			name = name.replaceAll("</b>", "");
			name = name.replaceAll("&nbsp;", "");
		}

		this.connection = connection;
		this.diagramController = diagramController;
		this.transition = trans;
		transition.setText(name);
		setAutoFit(true);
		setAutoHeight();

		addDoubleClickHandler(new DoubleClickHandler() {
			@Override
			public void onDoubleClick(DoubleClickEvent event) {
				if (readonly)
					return;

				Menu contextMenu = new Menu();

				MenuItem edit = new MenuItem();
				edit.setTitle(I18N.message("edit"));
				edit.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
					public void onClick(MenuItemClickEvent event) {
						edit();
					}
				});

				MenuItem delete = new MenuItem();
				delete.setTitle(I18N.message("ddelete"));
				delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
					public void onClick(MenuItemClickEvent event) {
						LD.ask(I18N.message("ddelete"), I18N.message("confirmdelete"), new BooleanCallback() {
							@Override
							public void execute(Boolean value) {
								if (value)
									delete();
							}
						});
					}
				});

				MenuItem makeStart = new MenuItem();
				makeStart.setTitle(I18N.message("startstate"));
				makeStart.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
					public void onClick(MenuItemClickEvent event) {
						makeStartState();
					}
				});

				MenuItem straight = new MenuItem();
				straight.setTitle(I18N.message("straight"));
				straight.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
					public void onClick(MenuItemClickEvent event) {
						restoreStraight();
					}
				});

				if (isTask())
					contextMenu.setItems(edit, makeStart, delete);
				else if (isConnection())
					contextMenu.setItems(edit, straight, delete);
				else
					contextMenu.setItems(edit, delete);
				contextMenu.showContextMenu();
				event.cancel();
			}
		});
	}

	/**
	 * Constructor used by new transitions.
	 */
	public StateWidget(Connection connection, DiagramController diagramController, String name) {
		this(connection, diagramController, new GUITransition(name));
	}

	public StateWidget(DrawingPanel dp, GUIWFState state) {
		this(null, dp.getDiagramController(), "<b>" + state.getName() + "</b>&nbsp;");
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
	}

	public void edit() {
		if (isTask() || isEnd()) {
			TaskDialog dialog = new TaskDialog(StateWidget.this);
			dialog.show();
		} else if (isJoin() || isFork()) {
			StatusDialog dialog = new StatusDialog(StateWidget.this);
			dialog.show();
		} else {
			TransitionDialog dialog = new TransitionDialog(StateWidget.this);
			dialog.show();
		}
	}

	public void delete() {
		if (wfState != null) {
			diagramController.deleteWidget(this);
		} else {
			connection.getStartShape().removeConnection(connection);
			diagramController.deleteConnection(connection);
		}
	}

	public void makeStartState() {
		WorkflowDesigner workflowDesigner = getDrawingPanel().getWorkflowDesigner();
		try {
			workflowDesigner.saveModel();
		} catch (Throwable t) {
		}

		workflowDesigner.getWorkflow().setStartStateId(wfState.getId());
		for (GUIWFState s : workflowDesigner.getWorkflow().getStates()) {
			s.setInitial(false);
		}
		getWfState().setInitial(true);
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

	public GUIWFState getWfState() {
		return wfState;
	}

	public DrawingPanel getDrawingPanel() {
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
}