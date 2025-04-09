package com.logicaldoc.gui.frontend.client.ai.robot;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * This widget allows for the selection of a robot
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class RobotSelector extends SelectItem {

	private static final String LABEL = "label";

	public RobotSelector() {
		setName("robot");
		setTitle(I18N.message("robot"));
		setWrapTitle(false);

		RobotListGridField label = new RobotListGridField(LABEL);

		setValueField("id");
		setDisplayField(LABEL);
		setSortField(LABEL);
		setPickListWidth(150);
		setPickListFields(label);

		setOptionDataSource(new RobotsDS());
		setHintStyle("hint");
	}

	public GUIRobot getRobot() {
		GUIRobot robot = null;
		ListGridRecord selection = getSelectedRecord();
		if (selection != null) {
			robot = new GUIRobot();
			robot.setId(selection.getAttributeAsLong("id"));
			robot.setName(selection.getAttributeAsString("name"));
			robot.setLabel(selection.getAttributeAsString(LABEL));
			robot.setAvatar(selection.getAttributeAsString("avatar"));
		}
		return robot;
	}
}