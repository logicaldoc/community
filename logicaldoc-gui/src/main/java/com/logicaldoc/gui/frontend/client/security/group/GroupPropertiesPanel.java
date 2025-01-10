package com.logicaldoc.gui.frontend.client.security.group;

import com.logicaldoc.gui.common.client.beans.GUIGroup;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Shows group's standard properties
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GroupPropertiesPanel extends HLayout {
	private static final String INHERIT = "inherit";

	private DynamicForm form1 = new DynamicForm();

	private ValuesManager vm = new ValuesManager();

	private GUIGroup group;

	private ChangedHandler changedHandler;

	public GroupPropertiesPanel(GUIGroup group, ChangedHandler changedHandler) {
		if (group == null) {
			setMembers(GroupsPanel.SELECT_GROUP);
		} else {
			this.group = group;
			this.changedHandler = changedHandler;
			setWidth100();
			setHeight100();
			setMembersMargin(20);
			refresh();
		}
	}

	private void refresh() {
		boolean readonly = (changedHandler == null);
		vm.clearValues();
		vm.clearErrors(false);

		if (form1 != null)
			form1.destroy();

		if (Boolean.TRUE.equals(contains(form1)))
			removeChild(form1);
		form1 = new DynamicForm();
		form1.setValuesManager(vm);
		form1.setTitleOrientation(TitleOrientation.TOP);

		StaticTextItem id = ItemFactory.newStaticTextItem("id", Long.toString(group.getId()));

		TextItem name = ItemFactory.newSimpleTextItem("name", group.getName());
		if (readonly || group.getId() != 0) {
			// In case of already existing group we do not need to enforce any
			// validation
			name = ItemFactory.newTextItem("name", group.getName());
			name.setDisabled(true);
		}
		name.setRequired(true);
		if (!readonly)
			name.addChangedHandler(changedHandler);

		TextItem description = ItemFactory.newTextItem("description", group.getDescription());
		description.setDisabled(readonly);
		if (!readonly)
			description.addChangedHandler(changedHandler);

		SelectItem inherit = ItemFactory.newGroupSelector(INHERIT, "inheritgroup");
		inherit.setVisible(!readonly);
		if (!readonly)
			inherit.addChangedHandler(changedHandler);

		form1.setItems(id, name, description, inherit);
		addMember(form1);
	}

	boolean validate() {
		vm.validate();
		if (Boolean.FALSE.equals(vm.hasErrors())) {
			group.setDescription(vm.getValueAsString("description"));
			group.setName(vm.getValueAsString("name"));
			if (vm.getValue(INHERIT) != null)
				group.setInheritGroupId(Long.parseLong(vm.getValueAsString(INHERIT)));
			else
				group.setInheritGroupId(null);
		}
		return !vm.hasErrors();
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