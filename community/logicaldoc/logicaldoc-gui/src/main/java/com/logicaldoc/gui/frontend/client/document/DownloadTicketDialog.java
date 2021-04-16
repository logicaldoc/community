package com.logicaldoc.gui.frontend.client.document;

import java.util.Date;
import java.util.LinkedHashMap;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.CloseClickEvent;
import com.smartgwt.client.widgets.events.CloseClickHandler;
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
public class DownloadTicketDialog extends Window {

	private IButton save;

	private DynamicForm form;

	private GUIDocument document;

	public DownloadTicketDialog(GUIDocument document) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("createdownloadticket"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		this.document = document;

		save = new IButton(I18N.message("save"));
		save.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				onSave();
			}
		});

		prepareForm();

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);

		layout.addMember(form);
		layout.addMember(save);

		addCloseClickHandler(new CloseClickHandler() {
			@Override
			public void onCloseClick(CloseClickEvent event) {
				DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						destroy();
					}
				});
			}
		});

		addItem(layout);
	}

	private void prepareForm() {
		form = new DynamicForm();
		form.setAlign(Alignment.LEFT);
		form.setNumCols(4);

		SelectItem type = ItemFactory.newAliasTypeSelector();
		type.setName("type");
		type.setValue("");
		type.setEndRow(true);
		type.setColSpan(4);
		type.setWrapTitle(false);

		DateItem date = ItemFactory.newDateItem("date", I18N.message("expireson"));
		date.setEndRow(true);
		date.setColSpan(4);
		date.setWrapTitle(false);

		SpinnerItem maxDownloads = ItemFactory.newSpinnerItem("maxDownloads", I18N.message("maxdownloads"),
				(Integer) null);
		maxDownloads.setEndRow(true);
		maxDownloads.setColSpan(4);
		maxDownloads.setWrapTitle(false);
		maxDownloads.setRequired(false);
		maxDownloads.setMin(0);

		SpinnerItem duedateTimeItem = ItemFactory.newSpinnerItem("duedateNumber", I18N.message("expiresin"), 24);
		duedateTimeItem.setWrapTitle(false);
		duedateTimeItem.setDefaultValue(24);
		duedateTimeItem.setMin(0);
		SelectItem duedateTime = ItemFactory.newDueTimeSelector("duedateTime", "");
		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		map.put("hour", I18N.message("hours"));
		map.put("day", I18N.message("ddays"));
		duedateTime.setValueMap(map);
		duedateTime.setValue("hour");

		form.setItems(type, duedateTimeItem, duedateTime, date, maxDownloads);
	}

	public void onSave() {
		if (!form.validate())
			return;

		String suffix = form.getValue("type").toString();
		Date date = (Date) form.getValue("date");

		Integer expireHours = null;
		if (form.getValue("duedateNumber") != null)
			expireHours = Integer.parseInt(form.getValueAsString("duedateNumber"));
		if ("day".equals(form.getValueAsString("duedateTime")))
			expireHours = expireHours * 24;

		if (date == null && (expireHours == null || expireHours.intValue() < 1))
			SC.warn(I18N.message("providexepinfo"));

		Integer maxDownloads = null;
		String val = form.getValueAsString("maxDownloads");
		if (val != null && !val.trim().isEmpty() && !"0".equals(val.trim()))
			maxDownloads = Integer.parseInt(val.trim());

		DocumentService.Instance.get().createDownloadTicket(document.getId(), suffix, expireHours, date, maxDownloads,
				new AsyncCallback<String>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
						destroy();
					}

					@Override
					public void onSuccess(String url) {
						destroy();

						SC.confirm(I18N.message("event.dticket.created.short"),
								"<a href='" + url + "' target='_blank'>" + url + "</a>", null);
					}
				});
	}
}