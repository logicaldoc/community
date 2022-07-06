package com.logicaldoc.gui.frontend.client.metadata.template;

import java.util.Map;

import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * This panel shows the validation script of the template.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.1
 */
public class TemplateValidationPanel extends HLayout {

	protected DynamicForm form = new DynamicForm();

	protected ValuesManager vm = new ValuesManager();

	protected GUITemplate template;

	protected ChangedHandler changedHandler;

	private HLayout container = new HLayout();

	public TemplateValidationPanel(GUITemplate template, ChangedHandler changedHandler,
			TemplateDetailsPanel detailsPanel) {
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
		vm.clearValues();
		vm.clearErrors(false);

		if (form != null)
			form.destroy();

		if (contains(container))
			removeMember(container);

		container = new HLayout();
		container.setMembersMargin(5);

		prepareForm();

		addMember(container);
	}

	private void prepareForm() {
		form = new DynamicForm();
		form.setNumCols(1);
		form.setValuesManager(vm);
		form.setWidth100();
		form.setTitleOrientation(TitleOrientation.TOP);

		TextAreaItem validation = ItemFactory.newTextAreaItemForAutomation("validation", "validation",
				template.getValidation(), (!template.isReadonly() && template.isWrite()) ? changedHandler : null,
				false);
		validation.setDisabled(template.isReadonly() || !template.isWrite());
		validation.setWidth("*");
		
		form.setItems(validation);

		container.addMember(form);
	}

	@SuppressWarnings("unchecked")
	protected boolean validate() {
		Map<String, Object> values = (Map<String, Object>) vm.getValues();

		vm.validate();
		if (!vm.hasErrors()) {
			template.setValidation((String) values.get("validation"));
		}

		return !vm.hasErrors();
	}

	public GUITemplate getTemplate() {
		return template;
	}
}