package com.logicaldoc.gui.frontend.client.impex.syndication;

import java.util.Date;
import java.util.Map;

import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUISyndication;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.FolderChangeListener;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.smartgwt.client.types.DateDisplayFormat;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.KeyPressEvent;
import com.smartgwt.client.widgets.form.fields.events.KeyPressHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Shows syndication's standard properties and read-only data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1.2
 */
public class SyndicationStandardProperties extends SyndicationDetailsTab {
	private DynamicForm form = new DynamicForm();

	private HLayout formsContainer = new HLayout();

	private FolderSelector sourceSelector;

	public SyndicationStandardProperties(GUISyndication syndication, final ChangedHandler changedHandler) {
		super(syndication, changedHandler);
		setWidth100();
		setHeight100();

		setMembers(formsContainer);
		sourceSelector = new FolderSelector("source", false);
		sourceSelector.setRequired(true);
		sourceSelector.setWidth(250);
		sourceSelector.setTitle(I18N.message("sourcefolder"));
		if (syndication.getSourceFolder() != null)
			sourceSelector.setFolder(syndication.getSourceFolder());
		sourceSelector.addFolderChangeListener(new FolderChangeListener() {
			@Override
			public void onChanged(GUIFolder folder) {
				changedHandler.onChanged(null);
			}
		});

		refresh();
	}

	private void refresh() {
		form.clearValues();
		form.clearErrors(false);

		if (form != null)
			form.destroy();

		if (formsContainer.contains(form))
			formsContainer.removeChild(form);

		form = new DynamicForm();
		form.setNumCols(2);
		form.setTitleOrientation(TitleOrientation.TOP);

		TextItem name = ItemFactory.newSimpleTextItem("name", "name", syndication.getName());
		name.addChangedHandler(changedHandler);
		name.setRequired(true);
		name.setDisabled(syndication.getId() != 0L);

		TextItem targetPath = ItemFactory.newTextItem("targetPath", "targetpath", syndication.getTargetPath());
		targetPath.addChangedHandler(changedHandler);
		targetPath.setWidth(250);
		targetPath.setRequired(true);

		TextItem remoteUrl = ItemFactory.newTextItem("url", "remoteurl", syndication.getUrl());
		remoteUrl.addChangedHandler(changedHandler);
		remoteUrl.setWidth(250);
		remoteUrl.setRequired(true);

		TextItem username = ItemFactory.newTextItemPreventAutocomplete("username", "username",
				syndication.getUsername());
		username.addChangedHandler(changedHandler);

		/*
		 * Two invisible fields to 'mask' the real credentials to the browser
		 * and prevent it to auto-fill the username and password we really use.
		 */
		TextItem fakeUsername = ItemFactory.newTextItem("prevent_autofill", "prevent_autofill",
				syndication.getUsername());
		fakeUsername.setCellStyle("nodisplay");
		TextItem hiddenPassword = ItemFactory.newTextItem("password_hidden", "password_hidden",
				syndication.getPassword());
		hiddenPassword.setCellStyle("nodisplay");
		hiddenPassword.addChangedHandler(changedHandler);
		FormItem password = ItemFactory.newSafePasswordItem("password", I18N.message("password"),
				syndication.getPassword(), hiddenPassword, changedHandler);
		password.addChangedHandler(changedHandler);

		TextItem include = ItemFactory.newTextItem("include", "include", syndication.getIncludes());
		include.addChangedHandler(changedHandler);

		TextItem exclude = ItemFactory.newTextItem("exclude", "exclude", syndication.getExcludes());
		exclude.addChangedHandler(changedHandler);

		SpinnerItem maxPacketSize = ItemFactory.newSpinnerItem("maxPacketSize", "maxpacketsize",
				syndication.getMaxPacketSize());
		maxPacketSize.setRequired(true);
		maxPacketSize.setHint("KB");
		maxPacketSize.setWidth(100);
		maxPacketSize.setMin(1);
		maxPacketSize.setStep(1);
		maxPacketSize.addChangedHandler(changedHandler);

		SpinnerItem batch = ItemFactory.newSpinnerItem("batch", "batch", syndication.getBatch());
		batch.setRequired(true);
		batch.setWidth(100);
		batch.setMin(1);
		batch.setStep(1000);
		batch.addChangedHandler(changedHandler);

		SpinnerItem timeout = ItemFactory.newSpinnerItem("timeout", "timeout", syndication.getTimeout());
		timeout.setRequired(true);
		timeout.setWidth(100);
		timeout.setMin(60);
		timeout.setStep(10);
		timeout.setHint(I18N.message("seconds").toLowerCase());
		timeout.addChangedHandler(changedHandler);

		final DateItem startDate = ItemFactory.newDateItem("startdate", "earliestdate");
		startDate.addChangedHandler(changedHandler);
		startDate.setValue(syndication.getStartDate());
		startDate.setUseMask(false);
		startDate.setShowPickerIcon(true);
		startDate.setDateFormatter(DateDisplayFormat.TOEUROPEANSHORTDATE);
		startDate.addKeyPressHandler(new KeyPressHandler() {
			@Override
			public void onKeyPress(KeyPressEvent event) {
				if ("delete".equals(event.getKeyName().toLowerCase())) {
					startDate.clearValue();
					startDate.setValue((Date) null);
					changedHandler.onChanged(null);
				} else {
					changedHandler.onChanged(null);
				}
			}
		});

		RadioGroupItem replicateCustomId = ItemFactory.newBooleanSelector("replicatecustomid", "replicatecustomid");
		replicateCustomId.setRequired(true);
		replicateCustomId.setValue(syndication.getReplicateCustomId() == 1 ? "yes" : "no");
		replicateCustomId.addChangedHandler(changedHandler);

		form.setItems(name, sourceSelector, remoteUrl, targetPath, fakeUsername, hiddenPassword, username, password,
				include, exclude, maxPacketSize, batch, timeout, startDate, replicateCustomId);

		formsContainer.addMember(form);

	}

	@SuppressWarnings("unchecked")
	boolean validate() {
		Map<String, Object> values = (Map<String, Object>) form.getValues();
		form.validate();
		if (!form.hasErrors()) {
			syndication.setName((String) values.get("name"));
			syndication.setUsername((String) values.get("username"));
			syndication.setPassword((String) values.get("password"));
			syndication.setTargetPath((String) values.get("targetPath"));
			syndication.setUrl((String) values.get("url"));
			syndication.setSourceFolder(sourceSelector.getFolder());
			syndication.setIncludes((String) values.get("include"));
			syndication.setExcludes((String) values.get("exclude"));
			syndication.setPassword((String) values.get("password_hidden"));

			if (values.get("maxPacketSize") instanceof Long)
				syndication.setMaxPacketSize((Long) values.get("maxPacketSize"));
			else
				syndication.setMaxPacketSize(Long.parseLong(values.get("maxPacketSize").toString()));

			if (values.get("batch") instanceof Long)
				syndication.setBatch((Long) values.get("batch"));
			else
				syndication.setBatch(Long.parseLong(values.get("batch").toString()));

			if (values.get("timeout") instanceof Integer)
				syndication.setTimeout((Integer) values.get("timeout"));
			else
				syndication.setTimeout(Integer.parseInt(values.get("timeout").toString()));

			syndication.setStartDate((Date) values.get("startdate"));
			syndication.setReplicateCustomId("yes".equals(values.get("replicatecustomid")) ? 1 : 0);
		}
		return !form.hasErrors();
	}
}