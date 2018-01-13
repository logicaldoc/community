package com.logicaldoc.gui.frontend.client.document;

import java.util.Date;
import java.util.Map;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.KeyPressEvent;
import com.smartgwt.client.widgets.form.fields.events.KeyPressHandler;

/**
 * Shows document's publishing options.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class PublishingPanel extends DocumentDetailTab {
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

		if (contains(form1))
			removeChild(form1);
		form1 = new DynamicForm();
		form1.setValuesManager(vm);
		form1.setTitleOrientation(TitleOrientation.TOP);
		form1.setWrapItemTitles(false);

		RadioGroupItem published = ItemFactory.newBooleanSelector("published", "published");
		if (document.getPublished() != -1) {
			published.setRequired(true);
			published.setValue(document.getPublished() == 1 ? "yes" : "no");
		}
		published.setEndRow(true);
		published.addChangedHandler(changedHandler);

		final DateItem startPublishing = ItemFactory.newDateItem("startpublishing", "startpublishing");
		startPublishing.setValue(document.getStartPublishing());
		startPublishing.addChangedHandler(changedHandler);
		startPublishing.setDisabled(!updateEnabled);
		startPublishing.setRequired(document.getPublished() != -1);
		startPublishing.setUseMask(false);
		startPublishing.setShowPickerIcon(true);
		startPublishing.setWrapTitle(false);
		startPublishing.setEndRow(true);
		startPublishing.addKeyPressHandler(new KeyPressHandler() {
			@Override
			public void onKeyPress(KeyPressEvent event) {
				if ("backspace".equals(event.getKeyName().toLowerCase())
						|| "delete".equals(event.getKeyName().toLowerCase())) {
					startPublishing.clearValue();
					startPublishing.setValue((Date) null);
					changedHandler.onChanged(null);
				} else {
					changedHandler.onChanged(null);
				}
			}
		});

		final DateItem stopPublishing = ItemFactory.newDateItem("stoppublishing", "stoppublishing");
		stopPublishing.setValue(document.getStopPublishing());
		stopPublishing.addChangedHandler(changedHandler);
		stopPublishing.setDisabled(!updateEnabled);
		stopPublishing.setRequired(false);
		stopPublishing.setUseMask(false);
		stopPublishing.setShowPickerIcon(true);
		stopPublishing.setEndRow(true);
		stopPublishing.setWrapTitle(false);
		stopPublishing.addKeyPressHandler(new KeyPressHandler() {
			@Override
			public void onKeyPress(KeyPressEvent event) {
				if ("backspace".equals(event.getKeyName().toLowerCase())
						|| "delete".equals(event.getKeyName().toLowerCase())) {
					stopPublishing.clearValue();
					stopPublishing.setValue((Date) null);
					changedHandler.onChanged(null);
				} else {
					changedHandler.onChanged(null);
				}
			}
		});

		form1.setItems(published, startPublishing, stopPublishing);
		addMember(form1);
	}

	@SuppressWarnings("unchecked")
	public boolean validate() {
		Map<String, Object> values = (Map<String, Object>) vm.getValues();
		vm.validate();
		if (!vm.hasErrors()) {
			if (!"".equals(values.get("published")) && values.get("published") != null)
				document.setPublished("yes".equals(values.get("published")) ? 1 : 0);
			document.setStartPublishing((Date) values.get("startpublishing"));
			document.setStopPublishing((Date) values.get("stoppublishing"));
		}
		return !vm.hasErrors();
	}
}