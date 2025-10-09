package com.logicaldoc.gui.frontend.client.settings;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel shows the Folders settings.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class RepositoriesPanel extends AdminPanel {

	private DynamicForm foldersForm = new DynamicForm();

	public RepositoriesPanel() {
		super("repositories");
	}

	@Override
	public void onDraw() {
		body.setMembers(new StoresPanel());

		// The Folders Tab
		Tab foldersTab = new Tab();
		foldersTab.setTitle(I18N.message("folders"));
		foldersForm.setWidth(400);
		foldersForm.setColWidths(1, "*");
		foldersForm.setTitleOrientation(TitleOrientation.LEFT);
		foldersTab.setPane(foldersForm);

		tabs.addTab(foldersTab);

		SettingService.Instance.get().loadSettingsByNames(Arrays.asList("conf.dbdir", "conf.exportdir",
				"conf.importdir", "conf.logdir", "conf.plugindir", "conf.userdir"),
				new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(List<GUIParameter> folderParameters) {
						List<FormItem> items = new ArrayList<>();

						for (GUIParameter f : folderParameters) {
							TextItem item = ItemFactory.newTextItem(f.getName(),
									f.getName().substring(f.getName().indexOf('.') + 1), f.getValue());
							item.setValue(f.getValue());
							item.setRequired(true);
							item.setWidth(400);
							items.add(item);
						}

						ButtonItem save = new ButtonItem("save", I18N.message("save"));
						save.addClickHandler(event -> onSaveFolders());
						items.add(save);

						save.setDisabled(Session.get().isDemo() && Session.get().getUser().getId() == 1);

						foldersForm.setItems(items.toArray(new FormItem[0]));
					}
				});
	}

	private void onSaveFolders() {
		final List<GUIParameter> settings = new ArrayList<>();
		@SuppressWarnings("unchecked")
		Map<String, Object> values = foldersForm.getValues();
		for (Map.Entry<String, Object> entry : values.entrySet()) {
			String name = entry.getKey();
			if (!"save".equals(name))
				settings.add(new GUIParameter(ItemFactory.originalItemName(name), entry.getValue().toString().trim()));
		}

		SettingService.Instance.get().saveSettings(settings, new DefaultAsyncCallback<>() {
			@Override
			public void handleSuccess(Void arg) {
				GuiLog.info(I18N.message("settingssaved"), null);

				// Replicate the settings in the current session
				for (GUIParameter setting : settings)
					Session.get().setConfig(setting.getName(), setting.getValue());
			}
		});
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