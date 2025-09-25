package com.logicaldoc.gui.frontend.client.ai.robot;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.EditingTabSet;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel collects details about a robot
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class RobotDetailsPanel extends VLayout {

	private GUIRobot robot;

	private Layout propertiesTabPanel;

	private RobotProperties propertiesPanel;

	private Layout historyTabPanel;

	private RobotHistoryPanel historyPanel;

	private EditingTabSet tabSet;

	private RobotsPanel robotsPanel;

	public RobotDetailsPanel(RobotsPanel robotsPanel) {
		super();

		this.robotsPanel = robotsPanel;
		setHeight100();
		setWidth100();
		setMembersMargin(10);

		tabSet = new EditingTabSet(saveEvent -> onSave(), cancelEvent -> {
			if (robot.getId() != 0) {
				RobotService.Instance.get().get(robot.getId(), new DefaultAsyncCallback<>() {

					@Override
					public void onSuccess(GUIRobot robot) {
						setRobot(robot);
					}

				});
			} else {
				setRobot(new GUIRobot());
			}
			tabSet.hideSave();
		});

		propertiesTabPanel = new HLayout();
		propertiesTabPanel.setWidth100();
		propertiesTabPanel.setHeight100();
		Tab propertiesTab = new Tab(I18N.message("properties"));
		propertiesTab.setPane(propertiesTabPanel);
		tabSet.addTab(propertiesTab);

		historyTabPanel = new HLayout();
		historyTabPanel.setWidth100();
		historyTabPanel.setHeight100();
		Tab historyTab = new Tab(I18N.message("history"));
		historyTab.setPane(historyTabPanel);
		tabSet.addTab(historyTab);

		addMember(tabSet);
	}

	private void refresh() {
		tabSet.hideSave();

		/*
		 * Prepare the standard properties tab
		 */
		if (propertiesPanel != null) {
			propertiesPanel.destroy();
			if (Boolean.TRUE.equals(propertiesTabPanel.contains(propertiesPanel)))
				propertiesTabPanel.removeMember(propertiesPanel);
		}

		if (historyPanel != null) {
			historyPanel.destroy();
			if (Boolean.TRUE.equals(historyTabPanel.contains(historyPanel)))
				historyTabPanel.removeMember(historyPanel);
		}

		propertiesPanel = new RobotProperties(robot, event -> onModified());
		propertiesTabPanel.addMember(propertiesPanel);

		historyPanel = new RobotHistoryPanel(robot.getId());
		historyTabPanel.addMember(historyPanel);
	}

	public GUIRobot getModel() {
		return robot;
	}

	public void setRobot(GUIRobot robot) {
		this.robot = robot;
		refresh();
	}

	public void onModified() {
		tabSet.displaySave();
	}

	private boolean validate() {
		boolean valid = propertiesPanel.validate();
		if (!valid)
			tabSet.selectTab(0);

		return valid;
	}

	public void onSave() {
		if (validate()) {
			RobotService.Instance.get().save(robot, new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(GUIRobot robot) {
					tabSet.hideSave();
					if (robot != null) {
						robotsPanel.updateRecord(robot);
						robotsPanel.showRobotDetails(robot);
					}
				}
			});
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