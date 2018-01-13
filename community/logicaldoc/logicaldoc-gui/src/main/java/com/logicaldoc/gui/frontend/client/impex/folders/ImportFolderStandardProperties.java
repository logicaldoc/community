package com.logicaldoc.gui.frontend.client.impex.folders;

import java.util.Map;

import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIImportFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.FolderChangeListener;
import com.logicaldoc.gui.frontend.client.folder.FolderSelector;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Shows import folder's standard properties and read-only data
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class ImportFolderStandardProperties extends ImportFolderDetailsTab {
	private DynamicForm form = new DynamicForm();

	private HLayout formsContainer = new HLayout();

	private FolderSelector targetSelector;

	public ImportFolderStandardProperties(GUIImportFolder importFolder, final ChangedHandler changedHandler) {
		super(importFolder, changedHandler);
		setWidth100();
		setHeight100();

		setMembers(formsContainer);
		targetSelector = new FolderSelector("target", false);
		targetSelector.setRequired(true);
		targetSelector.setWidth(250);
		targetSelector.setTitle(I18N.message("target"));
		if (importFolder.getTarget() != null)
			targetSelector.setFolder(importFolder.getTarget());
		targetSelector.addFolderChangeListener(new FolderChangeListener() {
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

		TextItem path = ItemFactory.newTextItem("path", "path", importFolder.getPath());
		path.addChangedHandler(changedHandler);
		path.setWidth(250);
		path.setRequired(true);

		TextItem domain = ItemFactory.newTextItem("domain", "domain", importFolder.getDomain());
		domain.addChangedHandler(changedHandler);

		TextItem username = ItemFactory.newTextItem("username", "username", importFolder.getUsername());
		username.addChangedHandler(changedHandler);

		TextItem password = ItemFactory.newPasswordItem("password", "password", importFolder.getPassword());
		password.addChangedHandler(changedHandler);

		SelectItem language = ItemFactory.newLanguageSelector("language", false, false);
		language.addChangedHandler(changedHandler);
		language.setRequired(true);
		language.setValue(importFolder.getLanguage());

		TextItem include = ItemFactory.newTextItem("include", "include", importFolder.getIncludes());
		include.addChangedHandler(changedHandler);

		TextItem exclude = ItemFactory.newTextItem("exclude", "exclude", importFolder.getExcludes());
		exclude.addChangedHandler(changedHandler);

		if ("smb".equals(importFolder.getProvider()))
			form.setItems(path, targetSelector, language, domain, username, password, include, exclude);
		else
			form.setItems(path, targetSelector, language, include, exclude);

		formsContainer.addMember(form);
	}

	@SuppressWarnings("unchecked")
	boolean validate() {
		Map<String, Object> values = (Map<String, Object>) form.getValues();
		form.validate();
		if (!form.hasErrors()) {
			importFolder.setPath((String) values.get("path"));
			importFolder.setUsername((String) values.get("username"));
			importFolder.setPassword((String) values.get("password"));
			importFolder.setDomain((String) values.get("domain"));
			importFolder.setTarget(targetSelector.getFolder());
			importFolder.setLanguage((String) values.get("language"));
			importFolder.setIncludes((String) values.get("include"));
			importFolder.setExcludes((String) values.get("exclude"));
		}
		return !form.hasErrors();
	}
}