package com.logicaldoc.gui.frontend.client.ai.robot;

import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Superclass for all tab panels in the robot details area
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public abstract class RobotDetailsTab extends HLayout {
	protected GUIRobot robot;

	protected ChangedHandler changedHandler;

	/**
	 * Constructor
	 * 
	 * @param robot The robot this instance refers to
	 * @param changedHandler The handler to be invoked in case of changes in the
	 *        robot
	 */
	protected RobotDetailsTab(GUIRobot robot, ChangedHandler changedHandler) {
		super();
		this.robot = robot;
		this.changedHandler = changedHandler;
	}

	public GUIRobot getRobot() {
		return robot;
	}

	public ChangedHandler getChangedHandler() {
		return changedHandler;
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