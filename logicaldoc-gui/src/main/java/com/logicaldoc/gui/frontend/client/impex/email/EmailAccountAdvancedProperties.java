package com.logicaldoc.gui.frontend.client.impex.email;

import java.util.Date;
import java.util.LinkedHashMap;

import com.logicaldoc.gui.common.client.beans.GUIEmailAccount;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.DateDisplayFormat;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Shows account's advanced properties and read-only data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class EmailAccountAdvancedProperties extends EmailAccountDetailsTab {
	private static final String DELETE = "delete";

	private DynamicForm form = new DynamicForm();

	private HLayout formsContainer = new HLayout();

	public EmailAccountAdvancedProperties(GUIEmailAccount account, ChangedHandler changedHandler) {
		super(account, changedHandler);
		setWidth100();
		setHeight100();
		setMembers(formsContainer);
		refresh();
	}

	private void refresh() {
		form.clearValues();
		form.clearErrors(false);
		form.destroy();

		if (Boolean.TRUE.equals(formsContainer.contains(form)))
			formsContainer.removeChild(form);

		form = new DynamicForm();
		form.setNumCols(2);
		form.setTitleOrientation(TitleOrientation.TOP);

		TextItem include = ItemFactory.newTextItem("include", account.getIncludes());
		include.addChangedHandler(changedHandler);

		TextItem exclude = ItemFactory.newTextItem("exclude", account.getExcludes());
		exclude.addChangedHandler(changedHandler);

		TextItem folder = ItemFactory.newTextItem("mailfolder", account.getMailFolder());
		folder.addChangedHandler(changedHandler);

		SelectItem format = ItemFactory.newSelectItem("format");
		LinkedHashMap<String, String> map = new LinkedHashMap<>();
		map.put("0", I18N.message("multiplefiles"));
		map.put("1", I18N.message("singleeml"));
		format.setValueMap(map);
		format.addChangedHandler(changedHandler);
		format.setValue(Integer.toString(account.getFormat()));

		CheckboxItem deleteFomMailbox = new CheckboxItem();
		deleteFomMailbox.setName(DELETE);
		deleteFomMailbox.setTitle(I18N.message("deletefrommailbox"));
		deleteFomMailbox.setRedrawOnChange(true);
		deleteFomMailbox.setWidth(50);
		deleteFomMailbox.addChangedHandler(changedHandler);
		deleteFomMailbox.setValue(account.isDeleteFromMailbox());

		final DateItem startDate = ItemFactory.newDateItem("startdate", "earliestdate");
		startDate.addChangedHandler(changedHandler);
		startDate.setValue(account.getStartDate());
		startDate.setUseMask(false);
		startDate.setShowPickerIcon(true);
		startDate.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATE);
		startDate.addKeyPressHandler(event -> {
			if ("backspace".equalsIgnoreCase(event.getKeyName()) || DELETE.equalsIgnoreCase(event.getKeyName())) {
				startDate.clearValue();
				startDate.setValue((Date) null);
				changedHandler.onChanged(null);
			} else {
				changedHandler.onChanged(null);
			}
		});

		form.setItems(folder, format, include, exclude, startDate, deleteFomMailbox);

		formsContainer.addMember(form);
	}

	boolean validate() {
		if (form.validate()) {
			account.setIncludes(form.getValueAsString("include"));
			account.setExcludes(form.getValueAsString("exclude"));
			account.setDeleteFromMailbox(Boolean.parseBoolean(form.getValueAsString(DELETE)));
			account.setMailFolder(form.getValueAsString("mailfolder"));
			account.setFormat(Integer.parseInt(form.getValueAsString("format")));
			account.setStartDate((Date) form.getValue("startdate"));
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