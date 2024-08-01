package de.welcz.kotlinxserialization

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
class User(@SerialName("firstName") val first: String, @SerialName("lastName") val last: String)
