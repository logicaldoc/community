package com.logicaldoc.gui.frontend.client.impex.folders;

import java.util.Map;

import com.logicaldoc.gui.common.client.beans.GUIImportFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Shows import folder's standard properties and read-only data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ImportFolderStandardProperties extends ImportFolderDetailsTab {
	private static final String PASSWORD = "password";

	private static final String BATCH = "batch";

	private static final String USERNAME = "username";

	private static final String SERVER = "server";

	private static final String DOMAIN = "domain";

	private DynamicForm form = new DynamicForm();

	private HLayout formsContainer = new HLayout();

	private FolderSelector targetSelector;

	public ImportFolderStandardProperties(GUIImportFolder importFolder, final ChangedHandler changedHandler) {
		super(importFolder, changedHandler);
		setWidth100();
		setHeight100();

		setMembers(formsContainer);
		targetSelector = new FolderSelector("target", null);
		targetSelector.setRequired(true);
		targetSelector.setWidth(250);
		targetSelector.setTitle(I18N.message("targetfolder"));
		if (importFolder.getTarget() != null)
			targetSelector.setFolder(importFolder.getTarget());
		targetSelector.addFolderChangeListener(folder -> changedHandler.onChanged(null));

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

		SelectItem provider = ItemFactory.newImportFolderProviderOption(importFolder.getProvider());
		provider.addChangedHandler(new VisibilityChecker());
		provider.setRequired(true);

		TextItem path = ItemFactory.newTextItem("path", "sourcepath", importFolder.getPath());
		path.addChangedHandler(changedHandler);
		path.setWidth(250);
		path.setRequired(true);

		TextItem domain = ItemFactory.newTextItem(DOMAIN, importFolder.getDomain());
		domain.addChangedHandler(changedHandler);

		TextItem server = ItemFactory.newTextItem(SERVER, importFolder.getHost());
		server.addChangedHandler(changedHandler);

		IntegerItem port = ItemFactory.newIntegerItem("port", "port", importFolder.getPort());
		port.addChangedHandler(changedHandler);

		TextItem username = ItemFactory.newTextItemPreventAutocomplete(USERNAME, USERNAME, importFolder.getUsername());
		username.addChangedHandler(changedHandler);

		SelectItem language = ItemFactory.newLanguageSelector("language", false, false);
		language.addChangedHandler(changedHandler);
		language.setRequired(true);
		language.setValue(importFolder.getLanguage());

		TextItem include = ItemFactory.newTextItem("include", importFolder.getIncludes());
		include.addChangedHandler(changedHandler);

		TextItem exclude = ItemFactory.newTextItem("exclude", importFolder.getExcludes());
		exclude.addChangedHandler(changedHandler);

		SpinnerItem batch = ItemFactory.newSpinnerItem(BATCH, importFolder.getBatch());
		batch.setMin(1);
		batch.setStep(1000);
		batch.setRequired(true);
		batch.setWidth(100);
		batch.addChangedHandler(changedHandler);

		/*
		 * Two invisible fields to 'mask' the real credentials to the browser
		 * and prevent it to auto-fill the username and password we really use.
		 */
		TextItem fakeUsername = ItemFactory.newTextItem("prevent_autofill", importFolder.getUsername());
		fakeUsername.setCellStyle("nodisplay");
		TextItem hiddenPassword = ItemFactory.newTextItem("password_hidden", importFolder.getPassword());
		hiddenPassword.setCellStyle("nodisplay");
		hiddenPassword.addChangedHandler(changedHandler);
		FormItem password = ItemFactory.newSafePasswordItem(PASSWORD, I18N.message(PASSWORD),
				importFolder.getPassword(), hiddenPassword, changedHandler);
		password.addChangedHandler(changedHandler);

		form.setItems(provider, path, language, targetSelector, server, port, fakeUsername, hiddenPassword, username,
				password, domain, batch, include, exclude);

		formsContainer.addMember(form);

		onProviderChanged(importFolder.getProvider());
	}

	@SuppressWarnings("unchecked")
	boolean validate() {
		Map<String, Object> values = form.getValues();
		form.validate();
		if (Boolean.FALSE.equals(form.hasErrors())) {
			importFolder.setProvider((String) values.get("provider"));
			importFolder.setPath((String) values.get("path"));
			importFolder.setUsername((String) values.get(USERNAME));
			importFolder.setDomain((String) values.get(DOMAIN));
			importFolder.setTarget(targetSelector.getFolder());
			importFolder.setLanguage((String) values.get("language"));
			importFolder.setIncludes((String) values.get("include"));
			importFolder.setExcludes((String) values.get("exclude"));
			importFolder.setHost((String) values.get(SERVER));
			importFolder.setPort((Integer) values.get("port"));
			if (values.get(BATCH) instanceof Long longVal)
				importFolder.setBatch(longVal);
			else
				importFolder.setBatch(Long.valueOf(values.get(BATCH).toString()));

			importFolder.setPassword((String) values.get("password_hidden"));
		}
		return !form.hasErrors();
	}

	private void onProviderChanged(String provider) {
		form.hideItem(SERVER);
		form.hideItem("port");
		form.hideItem(USERNAME);
		form.hideItem(PASSWORD);
		form.hideItem(DOMAIN);

		if (provider.equals(GUIImportFolder.PROVIDER_FTP) || provider.equals(GUIImportFolder.PROVIDER_FTPS)
				|| provider.equals(GUIImportFolder.PROVIDER_SFTP)) {
			form.showItem(SERVER);
			form.showItem("port");
			form.showItem(USERNAME);
			form.showItem(PASSWORD);
		} else if (provider.startsWith(GUIImportFolder.PROVIDER_SMB)) {
			form.showItem(DOMAIN);
			form.showItem(USERNAME);
			form.showItem(PASSWORD);
		}
	}

	private class VisibilityChecker implements ChangedHandler {

		@Override
		public void onChanged(ChangedEvent event) {
			changedHandler.onChanged(event);
			String type = event.getValue().toString();
			onProviderChanged(type);
		}
	}
}