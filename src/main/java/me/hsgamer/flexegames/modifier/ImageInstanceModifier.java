package me.hsgamer.flexegames.modifier;

import me.hsgamer.flexegames.api.modifier.InstanceModifier;
import me.hsgamer.flexegames.api.modifier.InstanceModifierProvider;
import me.hsgamer.flexegames.util.AssetUtil;
import me.hsgamer.flexegames.util.PosUtil;
import me.hsgamer.hscore.common.CollectionUtils;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.entity.Entity;
import net.minestom.server.entity.EntityType;
import net.minestom.server.entity.Player;
import net.minestom.server.entity.metadata.other.ItemFrameMeta;
import net.minestom.server.event.EventFilter;
import net.minestom.server.event.EventNode;
import net.minestom.server.event.instance.AddEntityToInstanceEvent;
import net.minestom.server.event.trait.InstanceEvent;
import net.minestom.server.instance.Instance;
import net.minestom.server.item.ItemStack;
import net.minestom.server.item.Material;
import net.minestom.server.item.metadata.MapMeta;
import net.minestom.server.map.framebuffers.LargeGraphics2DFramebuffer;
import net.minestom.server.network.packet.server.ServerPacket;

import javax.imageio.ImageIO;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.*;
import java.util.stream.IntStream;

public class ImageInstanceModifier implements InstanceModifierProvider {
    private static final int MAP_UNIT_LENGTH = 128;
    private final int width;
    private final int height;
    private final int startId;
    private final List<Pos[]> posList;
    private final LargeGraphics2DFramebuffer framebuffer;
    private final ItemFrameMeta.Orientation orientation;

    public ImageInstanceModifier(Map<String, Object> map) {
        double scaleX = Double.parseDouble(Objects.toString(map.get("scale-x"), "1"));
        double scaleY = Double.parseDouble(Objects.toString(map.get("scale-y"), "1"));
        List<String> stringList = CollectionUtils.createStringListFromObject(map.getOrDefault("pos", ""), true);
        List<Pos[]> list = new ArrayList<>();
        int rawWidth = 1;
        for (String posString : stringList) {
            String[] split = posString.split(";");
            Pos[] pos = new Pos[split.length];
            for (int i = 0; i < split.length; i++) {
                pos[i] = PosUtil.convert(split[i].trim()).orElse(null);
            }
            list.add(pos);
            rawWidth = Math.max(rawWidth, pos.length);
        }
        this.width = rawWidth;
        this.height = list.size();
        this.posList = list;
        File asset = AssetUtil.getAssetFile(Objects.toString(map.get("asset")));
        framebuffer = new LargeGraphics2DFramebuffer(width * MAP_UNIT_LENGTH, height * MAP_UNIT_LENGTH);
        try (InputStream inputStream = new FileInputStream(asset)) {
            BufferedImage image = ImageIO.read(inputStream);
            framebuffer.getRenderer().drawRenderedImage(image, AffineTransform.getScaleInstance(scaleX, scaleY));
        } catch (Exception e) {
            throw new IllegalArgumentException("Error when loading file " + asset.getName(), e);
        }
        startId = (int) map.getOrDefault("startId", 0);
        orientation = ItemFrameMeta.Orientation.valueOf(Objects.toString(map.getOrDefault("orientation", ItemFrameMeta.Orientation.NORTH.name())).toUpperCase());
    }

    @Override
    public InstanceModifier getInstanceModifier(Instance instance) {
        List<Entity> entityList = new ArrayList<>();
        ServerPacket[] packets = new ServerPacket[width * height];
        EventNode<InstanceEvent> eventNode = EventNode.type("image-" + UUID.randomUUID(), EventFilter.INSTANCE);
        eventNode.addListener(AddEntityToInstanceEvent.class, event -> {
            if (event.getEntity().getInstance() == null && event.getEntity() instanceof Player player) {
                player.sendPackets(packets);
            }
        });

        return new InstanceModifier() {
            @Override
            public void init() {
                IntStream.range(0, width * height).forEach(id -> {
                    int x = id / height;
                    int y = id % height;
                    Pos pos = posList.get(y)[x];
                    Entity itemFrame = new Entity(EntityType.ITEM_FRAME);
                    ItemFrameMeta meta = (ItemFrameMeta) itemFrame.getEntityMeta();
                    itemFrame.setInstance(instance, pos);
                    meta.setNotifyAboutChanges(false);
                    meta.setOrientation(orientation);
                    meta.setInvisible(true);
                    meta.setItem(ItemStack.builder(Material.FILLED_MAP).meta(MapMeta.class, builder -> builder.mapId(startId + id)).build());
                    meta.setNotifyAboutChanges(true);
                    entityList.add(itemFrame);
                    packets[id] = framebuffer.createSubView(x * MAP_UNIT_LENGTH, y * MAP_UNIT_LENGTH).preparePacket(startId + id);
                });
                instance.eventNode().addChild(eventNode);
            }

            @Override
            public void clear() {
                entityList.forEach(Entity::remove);
                instance.eventNode().removeChild(eventNode);
            }
        };
    }
}
