package com.logicaldoc.gui.frontend.client.system;

import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
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
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * The Clustering console.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public class ClusteringPanel extends AdminPanel {

	private ValuesManager vm = new ValuesManager();

	public ClusteringPanel(GUIParameter[] parameters) {
		super("clustering");

		DynamicForm clusterForm = new DynamicForm();
		clusterForm.setWidth(300);
		clusterForm.setColWidths(1, "*");
		clusterForm.setValuesManager(vm);
		clusterForm.setTitleOrientation(TitleOrientation.LEFT);

		RadioGroupItem enabled = ItemFactory.newBooleanSelector("eenabled", "enabled");
		enabled.setValue("true".equals(parameters[0].getValue()) ? "yes" : "no");

		TextItem name = ItemFactory.newTextItem("name", I18N.message("name"), parameters[1].getValue());
		name.setRequired(true);

		TextItem host = ItemFactory.newTextItem("host", I18N.message("host"), parameters[2].getValue());
		host.setRequired(true);

		IntegerItem port = ItemFactory.newIntegerItem("port", I18N.message("port"),
				Integer.parseInt(parameters[3].getValue()));
		port.setRequired(true);

		TextItem context = ItemFactory.newTextItem("context", I18N.message("contextpath"), parameters[4].getValue());
		context.setRequired(true);

		IntegerItem baseport = ItemFactory.newIntegerItem("baseport", I18N.message("baseport"),
				Integer.parseInt(parameters[5].getValue()));
		baseport.setRequired(true);

		TextItem multicastip = ItemFactory.newTextItem("multicastip", I18N.message("multicastip"),
				parameters[6].getValue());

		StaticTextItem id = ItemFactory.newStaticTextItem("id", I18N.message("nodeid"), parameters[7].getValue());
		id.setWrap(false);
		id.setWrapTitle(false);

		ButtonItem save = new ButtonItem();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			@SuppressWarnings("unchecked")
			public void onClick(ClickEvent event) {
				final Map<String, Object> values = vm.getValues();

				if (vm.validate()) {
					final GUIParameter[] settings = new GUIParameter[7];
					settings[0] = new GUIParameter("cluster.enabled", values.get("eenabled").equals("yes") ? "true"
							: "false");
					settings[1] = new GUIParameter("cluster.name", vm.getValueAsString("name"));
					settings[2] = new GUIParameter("cluster.node.host", vm.getValueAsString("host"));
					settings[3] = new GUIParameter("cluster.node.port", vm.getValueAsString("port"));
					settings[4] = new GUIParameter("cluster.node.context", vm.getValueAsString("context"));
					settings[5] = new GUIParameter("cluster.port", vm.getValueAsString("baseport"));
					settings[6] = new GUIParameter("cluster.multicastip", vm.getValueAsString("multicastip"));

					SettingService.Instance.get().saveSettings(settings, new AsyncCallback<Void>() {
						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(Void result) {
							GuiLog.info(I18N.message("settingssaved") + " " + I18N.message("needrestart"), null);
						}
					});
				}
			}
		});

		clusterForm.setItems(enabled, id, name, multicastip, baseport, host, port, context, save);
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
}