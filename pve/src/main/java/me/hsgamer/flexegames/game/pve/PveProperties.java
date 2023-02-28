package me.hsgamer.flexegames.game.pve;

import lombok.experimental.UtilityClass;
import me.hsgamer.flexegames.api.property.GamePropertyAdapter;
import me.hsgamer.flexegames.api.property.GamePropertyKeyValue;

@UtilityClass
public class PveProperties {
    public static final GamePropertyKeyValue<Boolean> LEGACY_PVP = new GamePropertyKeyValue<>("legacy-pvp", false, new GamePropertyAdapter<>(Boolean.class));
    public static final GamePropertyKeyValue<Boolean> TOUGH_MOB = new GamePropertyKeyValue<>("tough-mob", false, new GamePropertyAdapter<>(Boolean.class));
    public static final GamePropertyKeyValue<Boolean> HEAL_ON_REST = new GamePropertyKeyValue<>("heal-on-rest", true, new GamePropertyAdapter<>(Boolean.class));
    public static final GamePropertyKeyValue<String> KIT = new GamePropertyKeyValue<>("kit", "default", new GamePropertyAdapter<>(String.class));
}
