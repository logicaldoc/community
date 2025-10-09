package com.logicaldoc.gui.frontend.client.system;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.ToggleItem;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * The Clustering console.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public class ClusteringPanel extends AdminPanel {

	private static final String BASEPORT = "baseport";

	private ValuesManager vm = new ValuesManager();

	public ClusteringPanel() {
		super("clustering");
	}

	@Override
	protected void onDraw() {
		SettingService.Instance.get()
				.loadSettingsByNames(Arrays.asList("cluster.enabled", "cluster.name", "cluster.port",
						"cluster.multicastip", "cluster.cache.resources", "cluster.chunk.size", "id"),
						new DefaultAsyncCallback<>() {
							@Override
							public void handleSuccess(List<GUIParameter> parameters) {
								init(parameters);
							}
						});
	}

	private void init(List<GUIParameter> parameters) {
		DynamicForm clusterForm = new DynamicForm();
		clusterForm.setWidth(300);
		clusterForm.setColWidths(1, "*");
		clusterForm.setValuesManager(vm);
		clusterForm.setTitleOrientation(TitleOrientation.LEFT);

		ToggleItem enabled = ItemFactory.newToggleItem("eenabled", "enabled", parameters.get(0).getValueAsBoolean());

		TextItem name = ItemFactory.newTextItem("name", parameters.get(1).getValue());
		name.setRequired(true);

		IntegerItem baseport = ItemFactory.newIntegerItem(BASEPORT, I18N.message(BASEPORT),
				Integer.parseInt(parameters.get(2).getValue()));
		baseport.setRequired(true);

		TextItem multicastip = ItemFactory.newTextItem("multicastip", parameters.get(3).getValue());

		ToggleItem cacheResources = ItemFactory.newToggleItem("cacheResources", "cache",
				parameters.get(4).getValueAsBoolean());
		cacheResources.setHint(I18N.message("cachesresources"));

		SpinnerItem chunkSize = ItemFactory.newSpinnerItem("chunksize", Integer.valueOf(parameters.get(5).getValue()));
		chunkSize.setHint("MB");
		chunkSize.setRequired(true);
		chunkSize.setWrapTitle(false);
		chunkSize.setMin(1);
		chunkSize.setStep(10);

		StaticTextItem id = ItemFactory.newStaticTextItem("id", I18N.message("nodeid"), parameters.get(6).getValue());
		id.setWrap(false);
		id.setWrapTitle(false);

		ButtonItem save = new ButtonItem();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(click -> {
			if (Boolean.TRUE.equals(vm.validate())) {
				List<GUIParameter> settings = new ArrayList<>();
				settings.add(new GUIParameter("cluster.enabled", vm.getValueAsString("eenabled")));
				settings.add(new GUIParameter("cluster.name", vm.getValueAsString("name")));
				settings.add(new GUIParameter("cluster.port", vm.getValueAsString(BASEPORT)));
				settings.add(new GUIParameter("cluster.multicastip", vm.getValueAsString("multicastip")));
				settings.add(new GUIParameter("cluster.cache.resources", vm.getValueAsString("cacheResources")));
				settings.add(new GUIParameter("cluster.chunk.size", vm.getValueAsString("chunksize")));

				SettingService.Instance.get().saveSettings(settings, new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(Void result) {
						GuiLog.info(I18N.message("settingssaved") + " " + I18N.message("needrestart"), null);
					}
				});
			}
		});

		clusterForm.setItems(enabled, id, name, multicastip, baseport, chunkSize, cacheResources, save);
		body.setMembers(clusterForm);

		Tab channels = new Tab();
		channels.setTitle(I18N.message("channels"));
		channels.setPane(new ChannelsPanel());
		tabs.addTab(channels);

		Tab settings = new Tab();
		settings.setTitle(I18N.message("settings"));
		settings.setPane(new ScopedPropertiesPanel());
		tabs.addTab(settings);
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