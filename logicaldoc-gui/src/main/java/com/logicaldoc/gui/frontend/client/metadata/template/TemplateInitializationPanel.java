package com.logicaldoc.gui.frontend.client.metadata.template;

import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * This panel shows the initialization script of the template.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.2
 */
public class TemplateInitializationPanel extends HLayout {

	protected DynamicForm form = new DynamicForm();

	protected GUITemplate template;

	protected ChangedHandler changedHandler;

	private HLayout container = new HLayout();

	public TemplateInitializationPanel(GUITemplate template, ChangedHandler changedHandler) {
		if (template == null) {
			setMembers(TemplatesPanel.SELECT_TEMPLATE);
			return;
		}

		this.template = template;
		this.changedHandler = changedHandler;
		setWidth100();
		setHeight100();
		setMembersMargin(10);

		refresh();
	}

	private void refresh() {
		form.clearValues();
		form.clearErrors(true);

		if (Boolean.TRUE.equals(contains(container)))
			removeMember(container);

		container = new HLayout();
		container.setMembersMargin(5);

		prepareForm();

		addMember(container);
	}

	private void prepareForm() {
		form = new DynamicForm();
		form.setNumCols(1);
		form.setWidth100();
		form.setTitleOrientation(TitleOrientation.TOP);

		TextAreaItem initialization = ItemFactory.newTextAreaItemForAutomation("initialization",
				template.getValidation(), (!template.isReadonly() && template.isWrite()) ? changedHandler : null,
				false);
		initialization.setDisabled(template.isReadonly() || !template.isWrite());
		initialization.setWidth("*");

		form.setItems(initialization);

		container.addMember(form);
	}

	protected boolean validate() {
		if (form.validate())
			template.setInitialization(form.getValueAsString("initialization"));
		return !form.hasErrors();
	}

	public GUITemplate getTemplate() {
		return template;
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