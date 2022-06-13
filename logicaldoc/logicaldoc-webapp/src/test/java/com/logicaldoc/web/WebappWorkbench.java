package com.logicaldoc.web;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

public class WebappWorkbench {
	public static void main(String[] args) throws IOException {
		// String
		// gridSpec="({field:[{name:\"id\",visible:false,width:60},{name:\"thumbnail\",visible:false,width:200},{name:\"statusIcons\",width:110},{name:\"icon\",width:21},{name:\"filename\",width:200},{name:\"size\",width:70},{name:\"fileVersion\",width:70},{name:\"published\",width:120},{name:\"publisher\",visible:false,width:90},{name:\"workflowStatus\",width:150},{name:\"ext_Num1\",width:100},{name:\"rating\",visible:false,width:95},{name:\"ext_object\",width:100},{name:\"ext_Date\",visible:false,width:90},{name:\"ext_industries\",visible:false,width:100},{name:\"language\",visible:false,width:100},{name:\"ext_approver\",width:100},{name:\"lastModified\",visible:false,width:120},{name:\"type\",visible:false,width:55},{name:\"version\",visible:false,width:55},{name:\"creator\",visible:false,width:90},{name:\"created\",visible:false,width:120},{name:\"customId\",visible:false,width:110},{name:\"comment\",visible:false,width:300},{name:\"template\",visible:false,width:150},{name:\"startPublishing\",visible:false,width:120},{name:\"stopPublishing\",visible:false,width:120}],sort:{fieldName:\"published\",sortDir:\"descending\",sortSpecifiers:[{property:\"published\",direction:\"descending\"}]}})";
		String gridSpec = "({field:[{name:\"id\",visible:false,width:60},{name:\"thumbnail\",visible:false,width:200},{name:\"statusIcons\",width:110},{name:\"icon\",width:21},{name:\"filename\",width:200},{name:\"size\",width:70},{name:\"fileVersion\",width:70},{name:\"published\",width:120},{name:\"publisher\",visible:false,width:90},{name:\"workflowStatus\",width:150},{name:\"ext_Num1\",width:100},{name:\"rating\",visible:false,width:95},{name:\"ext_object\",width:100},{name:\"ext_Date\",visible:false,width:90},{name:\"ext_industries\",visible:false,width:100},{name:\"language\",visible:false,width:100},{name:\"ext_approver\",width:100},{name:\"lastModified\",visible:false,width:120},{name:\"type\",visible:false,width:55},{name:\"version\",visible:false,width:55},{name:\"creator\",visible:false,width:90},{name:\"created\",visible:false,width:120},{name:\"customId\",visible:false,width:110},{name:\"comment\",visible:false,width:300},{name:\"template\",visible:false,width:150},{name:\"startPublishing\",visible:false,width:120},{name:\"stopPublishing\",visible:false,width:120}],sort:{fieldName:\"published\",sortDir:\"descending\",sortSpecifiers:[{property:\"published\",direction:\"descending\"},{property:\"size\",direction:\"ascending\",_embeddedComponents_isc_MultiSortDialog_0_multiSortPanel_optionsGrid:null},{property:\"filename\",direction:\"ascending\"},{property:\"ext_object\",direction:\"ascending\"}]}})";
		if (gridSpec.startsWith("("))
			gridSpec = gridSpec.substring(1);
		if (gridSpec.endsWith(")"))
			gridSpec = gridSpec.substring(0, gridSpec.length() - 1);

		ObjectMapper mapper = new ObjectMapper();
		mapper.configure(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true);
		JsonNode grid = mapper.readTree(gridSpec);
		JsonNode sortSpecifiers = grid.get("sort").get("sortSpecifiers");
		for (JsonNode jsonNode : sortSpecifiers.findParents("property")) {
			String property = jsonNode.findValue("property").asText();
			String direction = jsonNode.findValue("direction").asText().equalsIgnoreCase("descending") ? "desc" : "asc";
			System.out.println(property + " > " + direction);
		}
	}
}
