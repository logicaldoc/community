package com.logicaldoc.gui.frontend.client.filler;

import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Displays the automation routine associated to the Filler
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.3
 */
public class FillerAutomationPanel extends FillerDetailsTab {

	private DynamicForm form = new DynamicForm();

	private HLayout container = new HLayout();

	public FillerAutomationPanel(GUIFiller filler, ChangedHandler changedHandler) {
		super(filler, changedHandler);
		setWidth100();
		setHeight100();
		setMembers(container);
	}

	@Override
	public void onDraw() {
		form.clearValues();
		form.clearErrors(false);
		form.destroy();

		if (Boolean.TRUE.equals(container.contains(form)))
			container.removeChild(form);

		form = new DynamicForm();
		form.setWidth100();
		form.setHeight100();
		form.setNumCols(1);
		form.setTitleOrientation(TitleOrientation.TOP);

		TextAreaItem automationBefore = ItemFactory.newTextAreaItemForAutomation("automationBefore", "automationbefore",
				filler.getAutomationBefore(), changedHandler, false);
		automationBefore.setRequired(false);
		automationBefore.setWidth("*");
		automationBefore.setHeight("*");
		automationBefore.addChangedHandler(changedHandler);

		TextAreaItem automation = ItemFactory.newTextAreaItemForAutomation("automation", "automation",
				filler.getAutomation(), changedHandler, false);
		automation.setRequired(false);
		automation.setWidth("*");
		automation.setHeight("*");
		automation.addChangedHandler(changedHandler);

		form.setItems(automationBefore, automation);

		container.addMember(form);
	}

	boolean validate() {
		if (form.validate()) {
			filler.setAutomationBefore(form.getValueAsString("automationBefore"));
			filler.setAutomation(form.getValueAsString("automation"));
		}
		return !form.hasErrors();
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