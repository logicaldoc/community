package com.logicaldoc.gui.frontend.client.impex.email;

import java.util.Map;

import com.logicaldoc.gui.common.client.beans.GUIEmailAccount;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Displays the automation routine associated to the Email account
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.1
 */
public class EmailAccountAutomationPanel extends EmailAccountDetailsTab {

	private DynamicForm form = new DynamicForm();

	private HLayout container = new HLayout();

	public EmailAccountAutomationPanel(GUIEmailAccount account, ChangedHandler changedHandler) {
		super(account, changedHandler);
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

		TextAreaItem automationBefore = ItemFactory.newTextAreaItemForAutomation("automationBefore", "whenemailprocessing",
				account.getAutomation(), changedHandler, false);
		automationBefore.setRequired(false);
		automationBefore.setWidth("*");
		automationBefore.setHeight("*");
		automationBefore.addChangedHandler(changedHandler);

		TextAreaItem automationAfter = ItemFactory.newTextAreaItemForAutomation("automationAfter", "afteremailprocessed",
				account.getAutomationAfter(), changedHandler, false);
		automationAfter.setRequired(false);
		automationAfter.setWidth("*");
		automationAfter.setHeight("*");
		automationAfter.addChangedHandler(changedHandler);
		
		form.setItems(automationBefore, automationAfter);

		container.addMember(form);
	}

	@SuppressWarnings("unchecked")
	boolean validate() {
		Map<String, Object> values = (Map<String, Object>) form.getValues();
		form.validate();
		if (Boolean.FALSE.equals(form.hasErrors())) {
			account.setAutomation((String) values.get("automationBefore"));
			account.setAutomationAfter((String) values.get("automationAfter"));
		}
		return !form.hasErrors();
	}
}