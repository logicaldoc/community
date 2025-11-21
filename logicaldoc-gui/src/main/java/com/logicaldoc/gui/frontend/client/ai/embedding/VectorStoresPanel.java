package com.logicaldoc.gui.frontend.client.ai.embedding;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.ai.AIService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * A panel to handle the supported vector stores
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.2
 */
public class VectorStoresPanel extends VLayout {

	private ValuesManager vm = new ValuesManager();

	public VectorStoresPanel() {
		setMembersMargin(5);
		setMargin(5);
	}

	@Override
	public void onDraw() {
		AIService.Instance.get().loadVectorStore(new DefaultAsyncCallback<List<GUIParameter>>() {

			@Override
			protected void handleSuccess(List<GUIParameter> settings) {
				initGUI(settings);
			}
		});
	}

	void initGUI(List<GUIParameter> settings) {
		TextItem url = ItemFactory.newTextItem("url", Util.getValue("url", settings));
		url.setRequired(true);

		DynamicForm parametersForm = new DynamicForm();
		parametersForm.setValuesManager(vm);
		parametersForm.setTitleOrientation(TitleOrientation.LEFT);
		parametersForm.setNumCols(2);
		parametersForm.setColWidths(1, "*");
		parametersForm.setPadding(5);
		parametersForm.setItems(url);
		addMember(parametersForm);

		IButton save = new IButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				if (Boolean.TRUE.equals(vm.validate())) {
					List<GUIParameter> params = new ArrayList<>();

					params.add(new GUIParameter("url", vm.getValueAsString("url")));
					AIService.Instance.get().saveVectorStore(params, new DefaultAsyncCallback<Void>() {

						@Override
						protected void handleSuccess(Void result) {
							GuiLog.info(I18N.message("settingssaved"), null);
						}
					});
				}
			}
		});
		addMember(save);
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