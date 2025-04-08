package com.logicaldoc.gui.frontend.client.ai;

import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.frontend.client.administration.AdminScreen;
import com.logicaldoc.gui.frontend.client.ai.robot.RobotsPanel;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows the administration of AI aspects
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class AIMenu extends VLayout {

	public AIMenu() {
		setMargin(10);
		setMembersMargin(5);
		setOverflow(Overflow.AUTO);

		addModelsButton();
	}

	private void addModelsButton() {
		if (Menu.enabled(Menu.AI_MODELS)) {
			Button models = new Button(I18N.message("models"));
			models.setWidth100();
			models.setHeight(25);
			models.addClickHandler(click -> AdminScreen.get().setContent(new ModelsAndSamplersPanel()));
			addMember(models);
		}

		if (Menu.enabled(Menu.ROBOTS)) {
			Button robots = new Button(I18N.message("robots"));
			robots.setWidth100();
			robots.setHeight(25);
			robots.addClickHandler(click -> AdminScreen.get().setContent(new RobotsPanel()));
			addMember(robots);
		}
	}
}