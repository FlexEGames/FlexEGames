package me.hsgamer.flexegames.game.pve;

import lombok.experimental.UtilityClass;
import me.hsgamer.flexegames.api.property.PropertyAdapter;
import me.hsgamer.flexegames.api.property.PropertyKeyValue;

@UtilityClass
public class PveProperties {
    public static final PropertyKeyValue<Boolean> LEGACY_PVP = new PropertyKeyValue<>("legacy-pvp", false, new PropertyAdapter<>(Boolean.class));
    public static final PropertyKeyValue<Boolean> TOUGH_MOB = new PropertyKeyValue<>("tough-mob", false, new PropertyAdapter<>(Boolean.class));
    public static final PropertyKeyValue<Boolean> HEAL_ON_REST = new PropertyKeyValue<>("heal-on-rest", true, new PropertyAdapter<>(Boolean.class));
    public static final PropertyKeyValue<String> KIT = new PropertyKeyValue<>("kit", "default", new PropertyAdapter<>(String.class));
}
