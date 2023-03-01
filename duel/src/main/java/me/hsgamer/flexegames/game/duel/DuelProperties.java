package me.hsgamer.flexegames.game.duel;

import lombok.experimental.UtilityClass;
import me.hsgamer.flexegames.api.property.PropertyAdapter;
import me.hsgamer.flexegames.api.property.PropertyKeyValue;

@UtilityClass
public class DuelProperties {
    public static final PropertyKeyValue<String> WORLD = new PropertyKeyValue<>("world", "default", new PropertyAdapter<>(String.class));
    public static final PropertyKeyValue<Boolean> LEGACY_PVP = new PropertyKeyValue<>("legacy-pvp", false, new PropertyAdapter<>(Boolean.class));
    public static final PropertyKeyValue<String> KIT = new PropertyKeyValue<>("kit", "default", new PropertyAdapter<>(String.class));
}
