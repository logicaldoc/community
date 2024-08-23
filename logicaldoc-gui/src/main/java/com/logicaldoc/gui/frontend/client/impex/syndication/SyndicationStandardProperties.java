package com.logicaldoc.gui.frontend.client.impex.syndication;

import java.util.Date;
import java.util.Map;

import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUISyndication;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
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
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Shows syndication's standard properties and read-only data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1.2
 */
public class SyndicationStandardProperties extends SyndicationDetailsTab {
	private static final String NODISPLAY = "nodisplay";

	private static final String APIKEY = "apikey";

	private static final String REPLICATECUSTOMID = "replicatecustomid";

	private static final String TIMEOUT = "timeout";

	private static final String MAX_PACKET_SIZE = "maxpacketsize";

	private static final String USERNAME = "username";

	private static final String PASSWORD = "password";

	private static final String BATCH = "batch";

	private DynamicForm form = new DynamicForm();

	private HLayout formsContainer = new HLayout();

	private FolderSelector sourceSelector;

	public SyndicationStandardProperties(GUISyndication syndication, final ChangedHandler changedHandler) {
		super(syndication, changedHandler);
		setWidth100();
		setHeight100();

		setMembers(formsContainer);
		sourceSelector = new FolderSelector("source", null);
		sourceSelector.setRequired(true);
		sourceSelector.setWidth(250);
		sourceSelector.setTitle(I18N.message("sourcefolder"));
		if (syndication.getSourceFolder() != null)
			sourceSelector.setFolder(syndication.getSourceFolder());
		sourceSelector.addFolderChangeListener((GUIFolder folder) -> changedHandler.onChanged(null));

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

		TextItem name = ItemFactory.newSimpleTextItem("name", syndication.getName());
		name.addChangedHandler(changedHandler);
		name.setRequired(true);
		name.setDisabled(syndication.getId() != 0L);

		TextItem targetPath = ItemFactory.newTextItem("targetpath", syndication.getTargetPath());
		targetPath.addChangedHandler(changedHandler);
		targetPath.setWidth(250);
		targetPath.setRequired(true);

		TextItem remoteUrl = ItemFactory.newTextItem("url", "remoteurl", syndication.getUrl());
		remoteUrl.addChangedHandler(changedHandler);
		remoteUrl.setWidth(250);
		remoteUrl.setRequired(true);

		TextItem username = ItemFactory.newTextItemPreventAutocomplete(USERNAME, USERNAME, syndication.getUsername());
		username.addChangedHandler(changedHandler);

		/*
		 * Two invisible fields to 'mask' the real credentials to the browser
		 * and prevent it to auto-fill the username and password we really use.
		 */
		TextItem fakeUsername = ItemFactory.newTextItem("prevent_autofill", syndication.getUsername());
		fakeUsername.setCellStyle(NODISPLAY);
		TextItem hiddenPassword = ItemFactory.newTextItem("password_hidden", syndication.getPassword());
		hiddenPassword.setCellStyle(NODISPLAY);
		hiddenPassword.addChangedHandler(changedHandler);
		FormItem password = ItemFactory.newSafePasswordItem(PASSWORD, I18N.message(PASSWORD), syndication.getPassword(),
				hiddenPassword, changedHandler);
		password.addChangedHandler(changedHandler);
		
		TextItem hiddenApiKey = ItemFactory.newTextItem("apikey_hidden", syndication.getApiKey());
		hiddenApiKey.setCellStyle(NODISPLAY);
		hiddenApiKey.addChangedHandler(changedHandler);
		FormItem apiKey = ItemFactory.newSafePasswordItem(APIKEY, I18N.message(APIKEY), syndication.getApiKey(),
				hiddenApiKey, changedHandler);
		apiKey.addChangedHandler(changedHandler);

		TextItem include = ItemFactory.newTextItem("include", syndication.getIncludes());
		include.addChangedHandler(changedHandler);

		TextItem exclude = ItemFactory.newTextItem("exclude", syndication.getExcludes());
		exclude.addChangedHandler(changedHandler);

		SpinnerItem maxPacketSize = ItemFactory.newSpinnerItem(MAX_PACKET_SIZE, syndication.getMaxPacketSize());
		maxPacketSize.setRequired(true);
		maxPacketSize.setHint("KB");
		maxPacketSize.setWidth(100);
		maxPacketSize.setMin(1);
		maxPacketSize.setStep(1);
		maxPacketSize.addChangedHandler(changedHandler);

		SpinnerItem batch = ItemFactory.newSpinnerItem(BATCH, syndication.getBatch());
		batch.setRequired(true);
		batch.setWidth(100);
		batch.setMin(1);
		batch.setStep(1000);
		batch.addChangedHandler(changedHandler);

		SpinnerItem timeout = ItemFactory.newSpinnerItem(TIMEOUT, syndication.getTimeout());
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
		startDate.addKeyPressHandler(event -> {
			if ("delete".equalsIgnoreCase(event.getKeyName())) {
				startDate.clearValue();
				startDate.setValue((Date) null);
				changedHandler.onChanged(null);
			} else {
				changedHandler.onChanged(null);
			}
		});

		RadioGroupItem replicateCustomId = ItemFactory.newBooleanSelector(REPLICATECUSTOMID, REPLICATECUSTOMID);
		replicateCustomId.setRequired(true);
		replicateCustomId.setValue(syndication.getReplicateCustomId() == 1 ? "yes" : "no");
		replicateCustomId.addChangedHandler(changedHandler);

		form.setItems(name, sourceSelector, remoteUrl, targetPath, fakeUsername, hiddenPassword, username, password,
				apiKey, replicateCustomId, include, exclude, maxPacketSize, batch, timeout, startDate);

		formsContainer.addMember(form);

	}

	@SuppressWarnings("unchecked")
	boolean validate() {
		Map<String, Object> values = form.getValues();
		form.validate();
		if (Boolean.FALSE.equals(form.hasErrors())) {
			syndication.setName((String) values.get("name"));
			syndication.setUsername((String) values.get(USERNAME));
			syndication.setPassword((String) values.get(PASSWORD));
			syndication.setApiKey((String) values.get(APIKEY));
			syndication.setTargetPath((String) values.get("targetpath"));
			syndication.setUrl((String) values.get("url"));
			syndication.setSourceFolder(sourceSelector.getFolder());
			syndication.setIncludes((String) values.get("include"));
			syndication.setExcludes((String) values.get("exclude"));
			syndication.setPassword((String) values.get("password_hidden"));

			if (values.get(MAX_PACKET_SIZE) instanceof Long)
				syndication.setMaxPacketSize((Long) values.get(MAX_PACKET_SIZE));
			else
				syndication.setMaxPacketSize(Long.parseLong(values.get(MAX_PACKET_SIZE).toString()));

			if (values.get(BATCH) instanceof Long)
				syndication.setBatch((Long) values.get(BATCH));
			else
				syndication.setBatch(Long.parseLong(values.get(BATCH).toString()));

			if (values.get(TIMEOUT) instanceof Integer)
				syndication.setTimeout((Integer) values.get(TIMEOUT));
			else
				syndication.setTimeout(Integer.parseInt(values.get(TIMEOUT).toString()));

			syndication.setStartDate((Date) values.get("startdate"));
			syndication.setReplicateCustomId("yes".equals(values.get(REPLICATECUSTOMID)) ? 1 : 0);
		}
		return !form.hasErrors();
	}
}