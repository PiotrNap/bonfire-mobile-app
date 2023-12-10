import React, { useState } from "react"
import { View, Text, TouchableOpacity, StyleSheet } from "react-native"
import { Sizing, Typography } from "styles/index"
import { appContext } from "contexts/contextApi"
import { schemeBasedFontColor } from "../../styles/typography"
import { EventsList } from "components/booking/EventsList"
import { SlotsList } from "components/booking/SlotsList"

enum EventsTabsType {
  active_events,
  booked_slots,
  scheduled_slots,
}
type EventsTabParams = {
  reload: boolean
}

export const EventsTabs = ({ reload }: EventsTabParams) => {
  const { colorScheme } = appContext()
  const [activeTab, setActiveTab] = useState<EventsTabsType>(EventsTabsType.active_events)
  const eventsListRef = React.useRef<any>()

  return (
    <>
      <View style={styles.tabBar}>
        <TouchableOpacity
          style={[
            styles.tab,
            activeTab === EventsTabsType.active_events && styles.activeTab,
            { borderColor: schemeBasedFontColor(colorScheme) },
          ]}
          onPress={() => setActiveTab(EventsTabsType.active_events)}>
          <Text style={[styles.tabText, { color: schemeBasedFontColor(colorScheme) }]}>
            Active
          </Text>
        </TouchableOpacity>
        <TouchableOpacity
          style={[
            styles.tab,
            activeTab === EventsTabsType.booked_slots && styles.activeTab,
            { borderColor: schemeBasedFontColor(colorScheme) },
          ]}
          onPress={() => setActiveTab(EventsTabsType.booked_slots)}>
          <Text style={[styles.tabText, { color: schemeBasedFontColor(colorScheme) }]}>
            Booked
          </Text>
        </TouchableOpacity>
        <TouchableOpacity
          style={[
            styles.tab,
            activeTab === EventsTabsType.scheduled_slots && styles.activeTab,
            { borderColor: schemeBasedFontColor(colorScheme) },
          ]}
          onPress={() => setActiveTab(EventsTabsType.scheduled_slots)}>
          <Text style={[styles.tabText, { color: schemeBasedFontColor(colorScheme) }]}>
            Scheduled
          </Text>
        </TouchableOpacity>
      </View>
      <View style={styles.tabContent}>
        {activeTab === EventsTabsType.active_events && (
          <EventsList reload ref={eventsListRef} isOrganizerOwnEvents />
        )}
        {activeTab === EventsTabsType.booked_slots && (
          <SlotsList reload listType={"bookedSlots"} />
        )}
        {activeTab === EventsTabsType.scheduled_slots && (
          <SlotsList reload listType={"scheduledSlots"} />
        )}
      </View>
    </>
  )
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
  },
  tabBar: {
    flexDirection: "row",
    borderBottomWidth: 1,
    borderBottomColor: "#ccc",
  },
  tab: {
    flex: 1,
    padding: Sizing.x15,
    alignItems: "center",
    justifyContent: "center",
  },
  activeTab: {
    borderBottomWidth: 2,
    borderBottomColor: "#000",
  },
  tabContent: {
    flex: 1,
    width: "100%",
  },
  tabText: {
    ...Typography.header.x20,
  },
})
