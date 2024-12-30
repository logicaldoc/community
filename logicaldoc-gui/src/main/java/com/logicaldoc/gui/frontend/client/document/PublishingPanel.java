package com.logicaldoc.gui.frontend.client.document;

import java.util.Date;
import java.util.Map;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.ToggleItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;

/**
 * Shows document's publishing options.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class PublishingPanel extends DocumentDetailTab {
	private static final String PUBLISHED = "published";

	private DynamicForm form1 = new DynamicForm();

	private ValuesManager vm = new ValuesManager();

	public PublishingPanel(GUIDocument document, ChangedHandler changedHandler) {
		super(document, changedHandler);
		setWidth100();
		setHeight100();
		setMembersMargin(20);
		refresh();
	}

	private void refresh() {
		vm.clearValues();
		vm.clearErrors(false);

		if (form1 != null)
			form1.destroy();

		if (Boolean.TRUE.equals(contains(form1)))
			removeChild(form1);
		form1 = new DynamicForm();
		form1.setValuesManager(vm);
		form1.setTitleOrientation(TitleOrientation.TOP);
		form1.setWrapItemTitles(false);

		ToggleItem published = ItemFactory.newToggleItem(PUBLISHED, true);
		if (document.getPublished() != -1) {
			published.setRequired(true);
			published.setValue(document.getPublished() == 1);
		}
		published.setEndRow(true);
		published.addChangedHandler(changedHandler);

		final DateItem startPublishing = ItemFactory.newDateItem("startpublishing");
		startPublishing.setValue(document.getStartPublishing());
		startPublishing.addChangedHandler(changedHandler);
		startPublishing.setDisabled(!updateEnabled);
		startPublishing.setRequired(document.getPublished() != -1);
		startPublishing.setUseMask(false);
		startPublishing.setShowPickerIcon(true);
		startPublishing.setWrapTitle(false);
		startPublishing.setEndRow(true);
		startPublishing.addKeyPressHandler(event -> {
			if ("backspace".equalsIgnoreCase(event.getKeyName()) || "delete".equalsIgnoreCase(event.getKeyName())) {
				startPublishing.clearValue();
				startPublishing.setValue((Date) null);
				changedHandler.onChanged(null);
			} else {
				changedHandler.onChanged(null);
			}
		});

		final DateItem stopPublishing = ItemFactory.newDateItem("stoppublishing");
		stopPublishing.setValue(document.getStopPublishing());
		stopPublishing.addChangedHandler(changedHandler);
		stopPublishing.setDisabled(!updateEnabled);
		stopPublishing.setRequired(false);
		stopPublishing.setUseMask(false);
		stopPublishing.setShowPickerIcon(true);
		stopPublishing.setEndRow(true);
		stopPublishing.setWrapTitle(false);
		stopPublishing.addKeyPressHandler(event -> {
			if ("backspace".equalsIgnoreCase(event.getKeyName()) || "delete".equalsIgnoreCase(event.getKeyName())) {
				stopPublishing.clearValue();
				stopPublishing.setValue((Date) null);
				changedHandler.onChanged(null);
			} else {
				changedHandler.onChanged(null);
			}
		});

		form1.setItems(published, startPublishing, stopPublishing);
		addMember(form1);
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean validate() {
		Map<String, Object> values = vm.getValues();
		vm.validate();
		if (Boolean.FALSE.equals(vm.hasErrors())) {
			document.setPublished(Boolean.valueOf(vm.getValueAsString(PUBLISHED)) ? 1 : 0);
			document.setStartPublishing((Date) values.get("startpublishing"));
			document.setStopPublishing((Date) values.get("stoppublishing"));
		}
		return !vm.hasErrors();
	}
}