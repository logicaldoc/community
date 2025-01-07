package com.logicaldoc.gui.frontend.client.document;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to get details for the download ticket creation
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.4.1
 */
public class TicketDialog extends Window {

	private static final String MAXVIEWS = "maxviews";

	private static final String ACTION = "action";

	private static final String MAXDOWNLOADS = "maxdownloads";

	private static final String CONTENT = "content";

	private static final String DUEDATENUMBER = "duedatenumber";

	private IButton save;

	private DynamicForm form;

	private GUIDocument document;

	public TicketDialog(GUIDocument document) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("ticket"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		this.document = document;

		save = new IButton(I18N.message("save"));
		save.addClickHandler(event -> onSave());

		prepareForm();

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);

		layout.addMember(form);
		layout.addMember(save);

		addItem(layout);
	}

	private void prepareForm() {
		form = new DynamicForm();
		form.setAlign(Alignment.LEFT);
		form.setNumCols(4);

		SelectItem docOrPdfConversion = ItemFactory.newAliasTypeSelector();
		docOrPdfConversion.setTitle(I18N.message(CONTENT));
		docOrPdfConversion.setName(CONTENT);
		docOrPdfConversion.setValue("");
		docOrPdfConversion.setEndRow(true);
		docOrPdfConversion.setColSpan(4);
		docOrPdfConversion.setWrapTitle(false);

		DateItem date = ItemFactory.newDateItem("date", I18N.message("expireson"));
		date.setEndRow(true);
		date.setColSpan(4);
		date.setWrapTitle(false);

		SpinnerItem maxDownloads = ItemFactory.newSpinnerItem(MAXDOWNLOADS, (Integer) null);
		maxDownloads.setEndRow(true);
		maxDownloads.setColSpan(4);
		maxDownloads.setWrapTitle(false);
		maxDownloads.setRequired(false);
		maxDownloads.setMin(0);

		SpinnerItem duedateTimeItem = ItemFactory.newSpinnerItem(DUEDATENUMBER, I18N.message("expiresin"), 24);
		duedateTimeItem.setWrapTitle(false);
		duedateTimeItem.setDefaultValue(24);
		duedateTimeItem.setMin(0);
		SelectItem duedateTime = ItemFactory.newDueTimeSelector("duedatetime", "");
		LinkedHashMap<String, String> map = new LinkedHashMap<>();
		map.put("hour", I18N.message("hours"));
		map.put("day", I18N.message("ddays"));
		duedateTime.setValueMap(map);
		duedateTime.setValue("hour");

		SelectItem action = ItemFactory.newSelectItem(ACTION);
		action.setEndRow(true);
		action.setColSpan(4);
		action.setWrapTitle(false);
		LinkedHashMap<String, String> types = new LinkedHashMap<>();
		types.put("0", I18N.message("download"));
		types.put("2", I18N.message("view"));
		action.setValueMap(types);
		action.setValue("0");
		action.setVisible(Feature.enabled(Feature.VIEW_TICKET));

		SpinnerItem maxViews = ItemFactory.newSpinnerItem(MAXVIEWS, (Integer) null);
		maxViews.setEndRow(true);
		maxViews.setColSpan(4);
		maxViews.setWrapTitle(false);
		maxViews.setRequired(false);
		maxViews.setMin(0);
		maxViews.setVisibleWhen(new AdvancedCriteria(ACTION, OperatorId.EQUALS, "2"));

		form.setItems(action, docOrPdfConversion, duedateTimeItem, duedateTime, date, maxDownloads, maxViews);
	}

	public void onSave() {
		if (!form.validate())
			return;

		String suffix = form.getValue(CONTENT).toString();
		Date date = (Date) form.getValue("date");

		Integer expireHours = null;
		if (form.getValue(DUEDATENUMBER) != null)
			expireHours = Integer.parseInt(form.getValueAsString(DUEDATENUMBER));
		if (expireHours != null && "day".equals(form.getValueAsString("duedatetime")))
			expireHours = expireHours * 24;

		if (date == null && (expireHours == null || expireHours.intValue() < 1))
			SC.warn(I18N.message("providexepinfo"));

		Integer maxDownloads = null;
		String val = form.getValueAsString(MAXDOWNLOADS);
		if (val != null && !val.trim().isEmpty())
			maxDownloads = Integer.parseInt(val.trim());

		Integer maxViews = null;
		val = form.getValueAsString(MAXVIEWS);
		if (val != null && !val.trim().isEmpty())
			maxViews = Integer.parseInt(val.trim());

		DocumentService.Instance.get().createDownloadTicket(document.getId(),
				Integer.parseInt(form.getValueAsString(ACTION)), suffix, expireHours, date, maxDownloads, maxViews,
				new DefaultAsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						super.onFailure(caught);
						destroy();
					}

					@Override
					public void onSuccess(List<String> ret) {
						destroy();
						new TicketDisplay(ret.get(0), ret.get(1), ret.get(2)).show();
					}
				});
	}
}