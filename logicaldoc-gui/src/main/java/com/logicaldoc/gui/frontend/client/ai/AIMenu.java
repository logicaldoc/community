package com.logicaldoc.gui.frontend.client.ai;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.frontend.client.administration.AdminScreen;
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
		Button templates = new Button(I18N.message("models"));
		templates.setWidth100();
		templates.setHeight(25);
		templates.addClickHandler(templatesClick -> AdminScreen.get().setContent(new ModelsAndSamplersPanel()));
		addMember(templates);
	}
}