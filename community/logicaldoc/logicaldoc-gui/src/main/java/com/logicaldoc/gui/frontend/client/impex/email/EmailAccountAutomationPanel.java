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

		if (form != null)
			form.destroy();

		if (container.contains(form))
			container.removeChild(form);

		form = new DynamicForm();
		form.setWidth100();
		form.setHeight100();
		form.setNumCols(1);
		form.setTitleOrientation(TitleOrientation.TOP);

		TextAreaItem automation = ItemFactory.newTextAreaItemForAutomation("automation", "automation",
				account.getAutomation(), changedHandler, false);
		automation.setShowTitle(false);
		automation.setStartRow(false);
		automation.setRequired(false);
		automation.setWidth("*");
		automation.setHeight("*");
		automation.addChangedHandler(changedHandler);

		form.setItems(automation);

		container.addMember(form);
	}

	@SuppressWarnings("unchecked")
	boolean validate() {
		Map<String, Object> values = (Map<String, Object>) form.getValues();
		form.validate();
		if (!form.hasErrors()) {
			account.setAutomation((String) values.get("automation"));
		}
		return !form.hasErrors();
	}
}